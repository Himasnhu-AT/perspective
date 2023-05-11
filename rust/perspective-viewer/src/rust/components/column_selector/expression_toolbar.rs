////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2018, the Perspective Authors.
//
// This file is part of the Perspective library, distributed under the terms
// of the Apache License 2.0.  The full license can be found in the LICENSE
// file.

use yew::prelude::*;

#[derive(PartialEq, Clone, Properties)]
pub struct ExprEditButtonProps {
    pub name: String,
    pub on_open_expr_panel: Callback<Option<String>>,
}

/// A button that goes into a column-list for a custom expression
/// when pressed, it opens up the expression editor side-panel.
#[function_component]
pub fn ExprEditButton(p: &ExprEditButtonProps) -> Html {
    let edit = Callback::from({
        let p = p.clone();
        move |_| {
            p.on_open_expr_panel.emit(Some(p.name.clone()));
        }
    });
    html! {
        <span onmousedown={ edit } class="expression-edit-button"></span>
    }
}
