////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2018, the Perspective Authors.
//
// This file is part of the Perspective library, distributed under the terms
// of the Apache License 2.0.  The full license can be found in the LICENSE
// file.

use std::rc::Rc;

use futures::channel::oneshot::*;
use wasm_bindgen::prelude::*;
use yew::prelude::*;

use super::column_selector::ColumnSelector;
use super::containers::split_panel::SplitPanel;
use super::font_loader::{FontLoader, FontLoaderProps, FontLoaderStatus};
use super::plugin_selector::PluginSelector;
use super::render_warning::RenderWarning;
use super::status_bar::StatusBar;
use super::style::{LocalStyle, StyleProvider};
use crate::components::expression_editor::ExpressionEditor;
use crate::config::*;
use crate::dragdrop::*;
use crate::model::*;
use crate::presentation::Presentation;
use crate::renderer::*;
use crate::session::*;
use crate::utils::*;
use crate::*;

#[derive(Properties)]
pub struct PerspectiveViewerProps {
    pub elem: web_sys::HtmlElement,
    pub session: Session,
    pub renderer: Renderer,
    pub presentation: Presentation,
    pub dragdrop: DragDrop,

    #[prop_or_default]
    pub weak_link: WeakScope<PerspectiveViewer>,
}

derive_model!(Renderer, Session for PerspectiveViewerProps);

impl PartialEq for PerspectiveViewerProps {
    fn eq(&self, _rhs: &Self) -> bool {
        false
    }
}

impl PerspectiveViewerProps {
    fn is_title(&self) -> bool {
        !self.presentation.get_is_workspace() && self.presentation.get_title().is_some()
    }
}

pub enum Msg {
    Resize,
    Reset(bool, Option<Sender<()>>),
    ToggleSettingsInit(Option<SettingsUpdate>, Option<Sender<ApiResult<JsValue>>>),
    ToggleSettingsComplete(SettingsUpdate, Sender<()>),
    PreloadFontsUpdate,
    RenderLimits(Option<(usize, usize, Option<usize>, Option<usize>)>),
    ExpressionEditor(Option<Option<String>>),
}

pub struct PerspectiveViewer {
    dimensions: Option<(usize, usize, Option<usize>, Option<usize>)>,
    on_rendered: Option<Sender<()>>,
    fonts: FontLoaderProps,
    settings_open: bool,
    /// Outer option is if it should be open or not
    /// inner option on which expr to use:
    /// None for a new expression,
    /// Some(s) for editing expr `s`.
    expr_editor: Option<Option<String>>,
    on_resize: Rc<PubSub<()>>,
    on_dimensions_reset: Rc<PubSub<()>>,
    _subscriptions: [Subscription; 1],
}

impl Component for PerspectiveViewer {
    type Message = Msg;
    type Properties = PerspectiveViewerProps;

    fn create(ctx: &Context<Self>) -> Self {
        *ctx.props().weak_link.borrow_mut() = Some(ctx.link().clone());
        let elem = ctx.props().elem.clone();
        let callback = ctx.link().callback(|()| Msg::PreloadFontsUpdate);
        let limit_sub = {
            let callback = ctx.link().callback(|x| Msg::RenderLimits(Some(x)));
            ctx.props().renderer.limits_changed.add_listener(callback)
        };

        Self {
            dimensions: None,
            on_rendered: None,
            fonts: FontLoaderProps::new(&elem, callback),
            settings_open: false,
            expr_editor: None,
            on_resize: Default::default(),
            on_dimensions_reset: Default::default(),
            _subscriptions: [limit_sub],
        }
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::PreloadFontsUpdate => true,
            Msg::Resize => {
                self.on_resize.emit_all(());
                false
            }
            Msg::Reset(all, sender) => {
                clone!(
                    ctx.props().renderer,
                    ctx.props().session,
                    ctx.props().presentation
                );
                ApiFuture::spawn(async move {
                    session.reset(all);
                    renderer.reset().await;
                    presentation.reset_available_themes(None).await;
                    let result = renderer.draw(session.validate().await?.create_view()).await;

                    if let Some(sender) = sender {
                        sender.send(()).unwrap();
                    }

                    result
                });

                false
            }
            Msg::ToggleSettingsInit(Some(SettingsUpdate::Missing), None) => false,
            Msg::ToggleSettingsInit(Some(SettingsUpdate::Missing), Some(resolve)) => {
                resolve.send(Ok(JsValue::UNDEFINED)).unwrap();
                false
            }
            Msg::ToggleSettingsInit(Some(SettingsUpdate::SetDefault), resolve) => {
                self.init_toggle_settings_task(ctx, Some(false), resolve);
                false
            }
            Msg::ToggleSettingsInit(Some(SettingsUpdate::Update(force)), resolve) => {
                self.init_toggle_settings_task(ctx, Some(force), resolve);
                false
            }
            Msg::ToggleSettingsInit(None, resolve) => {
                self.init_toggle_settings_task(ctx, None, resolve);
                false
            }
            Msg::ToggleSettingsComplete(SettingsUpdate::SetDefault, resolve)
                if self.settings_open =>
            {
                self.settings_open = false;
                self.on_rendered = Some(resolve);
                true
            }
            Msg::ToggleSettingsComplete(SettingsUpdate::Update(force), resolve)
                if force != self.settings_open =>
            {
                self.settings_open = force;
                self.on_rendered = Some(resolve);
                true
            }
            Msg::ToggleSettingsComplete(_, resolve)
                if matches!(self.fonts.get_status(), FontLoaderStatus::Finished) =>
            {
                resolve.send(()).expect("Orphan render");
                false
            }
            Msg::ToggleSettingsComplete(_, resolve) => {
                self.on_rendered = Some(resolve);
                true
            }
            Msg::RenderLimits(dimensions) => {
                if self.dimensions != dimensions {
                    self.dimensions = dimensions;
                    true
                } else {
                    false
                }
            }
            Msg::ExpressionEditor(new_state) => {
                if self.expr_editor != new_state {
                    self.expr_editor = new_state;
                    true
                } else {
                    false
                }
            }
        }
    }

    /// This top-level component is mounted to the Custom Element, so it has no
    /// API to provide props - but for sanity if needed, just return true on
    /// change.
    fn changed(&mut self, _ctx: &Context<Self>, _old: &Self::Properties) -> bool {
        true
    }

    /// On rendered call notify_resize().  This also triggers any registered
    /// async callbacks to the Custom Element API.
    fn rendered(&mut self, ctx: &Context<Self>, _first_render: bool) {
        ctx.props()
            .presentation
            .set_settings_open(Some(self.settings_open))
            .unwrap();

        if self.on_rendered.is_some()
            && matches!(self.fonts.get_status(), FontLoaderStatus::Finished)
        {
            self.on_rendered
                .take()
                .unwrap()
                .send(())
                .expect("Orphan render");
        }
    }

    /// `PerspectiveViewer` has two basic UI modes - "open" and "closed".
    // TODO these may be expensive to build because they will generate recursively
    // from `JsPerspectiveConfig` - they may need caching as in the JavaScript
    // version.
    fn view(&self, ctx: &Context<Self>) -> Html {
        let settings = ctx.link().callback(|_| Msg::ToggleSettingsInit(None, None));
        let on_close_settings = ctx
            .link()
            .callback(|()| Msg::ToggleSettingsInit(None, None));
        let mut class = classes!("settings-closed");
        if ctx.props().is_title() {
            class.push("titled");
        }

        let on_open_expr_panel = ctx.link().callback(|s| Msg::ExpressionEditor(Some(s)));

        html_template! {
            <StyleProvider>
                <LocalStyle href={ css!("viewer") } />
                // Have to check all the way up here instead of simply
                // in the html! macro, as conditional-compilation
                // interacts weirdly with .children (that SplitPanel uses).
                // https://github.com/yewstack/yew/issues/3256
                if self.settings_open {
                    if let Some(ref editor_state) = self.expr_editor {
                        <SplitPanel
                            id="app_panel"
                            reverse=true
                            on_reset={ self.on_dimensions_reset.callback() }
                            on_resize_finished={ ctx.props().render_callback() }>
                            <SettingsPanel
                                renderer={ &ctx.props().renderer }
                                dragdrop={ &ctx.props().dragdrop }
                                session={ &ctx.props().session }
                                on_resize={ &self.on_resize }
                                on_dimensions_reset={ &self.on_dimensions_reset }
                                on_reset={ self.on_dimensions_reset.callback() }
                                on_resize_finished={ ctx.props().render_callback() }
                                { on_open_expr_panel }
                                { on_close_settings }
                            ></SettingsPanel>
                            <ExprEditorPanel
                                session = { &ctx.props().session }
                                renderer = { &ctx.props().renderer }
                                expr_alias = { editor_state.clone() }
                                on_close = { ctx.link().callback(|_| Msg::ExpressionEditor(None)) }
                            ></ExprEditorPanel>
                            <div id="main_column">
                                <StatusBar
                                    id="status_bar"
                                    session={ &ctx.props().session }
                                    renderer={ &ctx.props().renderer }
                                    presentation={ &ctx.props().presentation }
                                    on_reset={ ctx.link().callback(|all| Msg::Reset(all, None)) }>
                                </StatusBar>
                                <div id="main_panel_container">
                                    <RenderWarning
                                        dimensions={ self.dimensions }
                                        session={ &ctx.props().session }
                                        renderer={ &ctx.props().renderer }>
                                    </RenderWarning>
                                    <slot></slot>
                                </div>
                            </div>
                        </SplitPanel>
                    } else {
                    <SplitPanel
                        id="app_panel"
                        reverse=true
                        on_reset={ self.on_dimensions_reset.callback() }
                        on_resize_finished={ ctx.props().render_callback() }>
                        <SettingsPanel
                            renderer={ &ctx.props().renderer }
                            dragdrop={ &ctx.props().dragdrop }
                            session={ &ctx.props().session }
                            on_resize={ &self.on_resize }
                            on_dimensions_reset={ &self.on_dimensions_reset }
                            on_reset={ self.on_dimensions_reset.callback() }
                            on_resize_finished={ ctx.props().render_callback() }
                            { on_open_expr_panel }
                            { on_close_settings }
                        ></SettingsPanel>
                        <div id="main_column">
                            <StatusBar
                                id="status_bar"
                                session={ &ctx.props().session }
                                renderer={ &ctx.props().renderer }
                                presentation={ &ctx.props().presentation }
                                on_reset={ ctx.link().callback(|all| Msg::Reset(all, None)) }>
                            </StatusBar>
                            <div id="main_panel_container">
                                <RenderWarning
                                    dimensions={ self.dimensions }
                                    session={ &ctx.props().session }
                                    renderer={ &ctx.props().renderer }>
                                </RenderWarning>
                                <slot></slot>
                            </div>
                        </div>
                    </SplitPanel>
                    }
                } else {
                    <RenderWarning
                        dimensions={ self.dimensions }
                        session={ &ctx.props().session }
                        renderer={ &ctx.props().renderer }>
                    </RenderWarning>
                    if ctx.props().is_title() {
                        <StatusBar
                            id="status_bar"
                            session={ &ctx.props().session }
                            renderer={ &ctx.props().renderer }
                            presentation={ &ctx.props().presentation }
                            on_reset={ ctx.link().callback(|all| Msg::Reset(all, None)) }>
                        </StatusBar>
                    }
                    <div id="main_panel_container" { class }>
                        <slot></slot>
                    </div>
                    if !ctx.props().presentation.get_is_workspace() {
                        <div
                            id="settings_button"
                            class={ if ctx.props().is_title() { "noselect button closed titled" } else { "noselect button closed" } }
                            onmousedown={ settings }>
                        </div>
                    }
                }
            </StyleProvider>
            <FontLoader ..self.fonts.clone()></FontLoader>
        }
    }

    fn destroy(&mut self, _ctx: &Context<Self>) {}
}

impl PerspectiveViewer {
    /// Toggle the settings, or force the settings panel either open (true) or
    /// closed (false) explicitly.  In order to reduce apparent
    /// screen-shear, `toggle_settings()` uses a somewhat complex render
    /// order:  it first resize the plugin's `<div>` without moving it,
    /// using `overflow: hidden` to hide the extra draw area;  then,
    /// after the _async_ drawing of the plugin is complete, it will send a
    /// message to complete the toggle action and re-render the element with
    /// the settings removed.
    ///
    /// # Arguments
    /// * `force` - Whether to explicitly set the settings panel state to
    ///   Open/Close (`Some(true)`/`Some(false)`), or to just toggle the current
    ///   state (`None`).
    fn init_toggle_settings_task(
        &mut self,
        ctx: &Context<Self>,
        force: Option<bool>,
        sender: Option<Sender<ApiResult<JsValue>>>,
    ) {
        let is_open = ctx.props().presentation.is_settings_open();
        match force {
            Some(force) if is_open == force => {
                if let Some(sender) = sender {
                    sender.send(Ok(JsValue::UNDEFINED)).unwrap();
                }
            }
            Some(_) | None => {
                let force = !is_open;
                let callback = ctx.link().callback(move |resolve| {
                    let update = SettingsUpdate::Update(force);
                    Msg::ToggleSettingsComplete(update, resolve)
                });

                clone!(ctx.props().renderer, ctx.props().session);
                ApiFuture::spawn(async move {
                    let result = if session.js_get_table().is_some() {
                        renderer.presize(force, callback.emit_async_safe()).await
                    } else {
                        callback.emit_async_safe().await?;
                        Ok(JsValue::UNDEFINED)
                    };

                    if let Some(sender) = sender {
                        // TODO shouldn't ignore, this should retry (?)
                        let msg = result.clone().ignore_view_delete();
                        sender.send(msg).into_apierror()?;
                    };

                    result
                });
            }
        };
    }
}

#[derive(Properties)]
struct SettingsPanelProps {
    pub session: Session,
    pub renderer: Renderer,
    pub dragdrop: DragDrop,
    pub on_resize: Rc<PubSub<()>>,
    pub on_dimensions_reset: Rc<PubSub<()>>,
    pub on_reset: Callback<()>,
    pub on_resize_finished: Callback<()>,
    pub on_close_settings: Callback<()>,
    pub on_open_expr_panel: Callback<Option<String>>,
}

impl PartialEq for SettingsPanelProps {
    fn eq(&self, _rhs: &Self) -> bool {
        true
    }
}

#[function_component]
fn SettingsPanel(props: &SettingsPanelProps) -> Html {
    html! {
        <div id="settings_panel" class="sidebar_column noselect split-panel orient-vertical">
            <SidebarCloseButton id={ "settings_close_button" } on_close_sidebar={ &props.on_close_settings }></SidebarCloseButton>
            <PluginSelector
                session={ &props.session }
                renderer={ &props.renderer }>
            </PluginSelector>
            <ColumnSelector
                dragdrop={ &props.dragdrop }
                renderer={ &props.renderer }
                session={ &props.session }
                on_resize={ &props.on_resize }
                on_open_expr_panel={ &props.on_open_expr_panel }
                on_dimensions_reset={ &props.on_dimensions_reset }>
            </ColumnSelector>
        </div>
    }
}

#[derive(PartialEq, Clone, Properties)]
struct ExprEditorPanelProps {
    pub session: Session,
    pub renderer: Renderer,
    /// The expression to edit.
    /// If None make a new expression
    /// If Some, it should be the alias of an existing expression.
    pub expr_alias: Option<String>,
    /// when this callback is called, the expression editor will close.
    pub on_close: Callback<()>,
}

derive_model!(Renderer, Session for ExprEditorPanelProps);

#[function_component]
fn ExprEditorPanel(p: &ExprEditorPanelProps) -> Html {
    // If the editor is currently validating the expression.
    let validating = yew::use_state_eq(|| false);
    // if expr_alias is Some, this is an edit,
    //      so use session.create_replace_expression_update
    // else, this is a creation, do regularly.
    let on_save = Callback::from({
        let p = p.clone();
        move |v| {
            if let Some(ref alias) = &p.expr_alias {
                update_expr(alias, &v, &p);
            } else {
                save_expr(v, &p);
            }
            p.on_close.emit(());
        }
    });
    let on_validate = Callback::from({
        let validating_state = validating.clone();
        move |b| validating_state.set(b)
    });
    let on_delete = Callback::from({
        let p = p.clone();
        move |()| {
            // Delete button should only appear while editing an existing expr,
            // but this is how we get the name anyways, and this
            // feels safer than an unwrap().
            if let Some(ref s) = p.expr_alias {
                delete_expr(s, &p);
            }
            p.on_close.emit(());
        }
    });

    // FIXME: `validating: bool` doesnt work?
    let validating = if *validating { Some("") } else { None };
    html! {
        <div class="sidebar_column expr_editor_column" { validating }>
            <SidebarCloseButton id={ "expr_editor_close_button" } on_close_sidebar={ &p.on_close }></SidebarCloseButton>
            <div id="expr_panel_header">
                <p id="expr_panel_header_title">{ "Edit Expression" }</p>
            </div>
            <div id="expr_panel_border">
            </div>
            <ExpressionEditor
                { on_save }
                { on_validate }
                { on_delete }
                session = { &p.session }
                // TODO: dont clone here (or above), may be expensive!
                alias = { p.expr_alias.clone() }>
            </ExpressionEditor>
        </div>
    }
}

#[derive(PartialEq, Clone, Properties)]
struct SidebarCloseButtonProps {
    on_close_sidebar: Callback<()>,
    id: AttrValue,
}

#[function_component]
fn SidebarCloseButton(p: &SidebarCloseButtonProps) -> Html {
    let onclick = Callback::from({
        let cb = p.on_close_sidebar.clone();
        move |_| cb.emit(())
    });
    let id = &p.id;
    html! {
        <div { onclick } { id } class="sidebar_close_button"></div>
    }
}

fn update_expr(name: &str, new_expression: &JsValue, props: &ExprEditorPanelProps) {
    let n = name.to_string();
    let exp = new_expression.clone();
    let sesh = props.session.clone();
    let props = props.clone();
    ApiFuture::spawn(async move {
        let update = sesh.create_replace_expression_update(&n, &exp).await;
        props.update_and_render(update).await?;
        Ok(())
    });
}

fn save_expr(expression: JsValue, props: &ExprEditorPanelProps) {
    let task = {
        let expression = expression.as_string().unwrap();
        let mut expressions = props.session.get_view_config().expressions.clone();
        expressions.retain(|x| x != &expression);
        expressions.push(expression);
        props.update_and_render(ViewConfigUpdate {
            expressions: Some(expressions),
            ..Default::default()
        })
    };

    ApiFuture::spawn(task);
}

fn delete_expr(expr_name: &str, props: &ExprEditorPanelProps) {
    let session = &props.session;
    let expression = session
        .metadata()
        .get_expression_by_alias(expr_name)
        .unwrap();

    let mut expressions = session.get_view_config().expressions.clone();
    expressions.retain(|x| x != &expression);
    let config = ViewConfigUpdate {
        expressions: Some(expressions),
        ..ViewConfigUpdate::default()
    };

    let task = props.update_and_render(config);
    ApiFuture::spawn(task);
}
