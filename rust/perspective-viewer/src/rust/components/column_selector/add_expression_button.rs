////////////////////////////////////////////////////////////////////////////////
//
//
// This file is part of the Perspective library, distributed under the terms
// of the Apache License 2.0.  The full license can be found in the LICENSE
// file.

use web_sys::*;
use yew::prelude::*;

use crate::renderer::Renderer;
use crate::session::Session;
use crate::*;

#[derive(Clone, PartialEq, Properties)]
pub struct AddExpressionButtonProps {
    pub session: Session,
    pub renderer: Renderer,
    pub on_open_expr_panel: Callback<Option<String>>,
}

derive_model!(Renderer, Session for AddExpressionButtonProps);

pub enum AddExpressionButtonMsg {
    OpenExpressionEditor(bool),
    MouseEnter(bool),
    MouseLeave,
}

use AddExpressionButtonMsg::*;

#[derive(Default)]
pub struct AddExpressionButton {
    noderef: NodeRef,
    mouseover: bool,
    mode_open: bool,
}

impl Component for AddExpressionButton {
    type Message = AddExpressionButtonMsg;
    type Properties = AddExpressionButtonProps;

    fn create(_ctx: &Context<Self>) -> Self {
        Self::default()
    }

    fn update(&mut self, ctx: &Context<Self>, msg: AddExpressionButtonMsg) -> bool {
        match msg {
            OpenExpressionEditor(_reset) => {
                self.mode_open = true;

                ctx.props().on_open_expr_panel.emit(None);
                true
            }
            MouseEnter(is_render) => {
                self.mouseover = is_render;
                is_render
            }
            MouseLeave => {
                self.mouseover = false;
                true
            }
        }
    }

    /// `onmouseover` is triggered incorrectly on the `DragTarget` of a
    /// drag/drop action after `DropEvent` has fired on the `DropTarget`,
    /// causing brief hover effects where the mouse _was_ before the action
    /// initiated. Various methods of correcting this were attempted, settling
    /// on a manual `dragdrop-hover` class toggle, using the `.which()` property
    /// to determine the mis-fired event.  This was determined experimentally -
    /// according to my read of the spec, this is a bug in Chrome.
    ///
    /// As a result there are 3 hover states `MouseEnter(true)`,
    /// `MouseEnter(false)`, and `MouseLeave`;  `MouseEnter(false)` can be
    /// replaced, but it causes an extra render of the DOM un-necessarily.
    fn view(&self, ctx: &Context<Self>) -> Html {
        let onmouseout = ctx.link().callback(|_| MouseLeave);
        let onmouseover = ctx
            .link()
            .callback(|event: MouseEvent| MouseEnter(event.which() == 0));

        let onmousedown = ctx.link().callback(|event: MouseEvent| {
            AddExpressionButtonMsg::OpenExpressionEditor(event.shift_key())
        });

        let mut class = classes!();
        if self.mouseover || self.mode_open {
            class.push("dragdrop-hover");
        }

        html! {
            <div
                id="add-expression"
                data-index="-1"
                ref={ &self.noderef }
                { class }
                { onmouseover }
                { onmouseout }
                { onmousedown }>

                <span id="add-expression-title">{ "New Column" }</span>
            </div>
        }
    }
}
