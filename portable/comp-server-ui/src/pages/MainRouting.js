import React from "react";
import {BrowserRouter, Route, Switch} from "react-router-dom";

import {NotFound} from "./NotFound";
import {UserPage} from "./UserPage";
import {AuthPage} from "./AuthPage";

export function MainRouting() {
    return (
        <BrowserRouter>
            <Switch>
                <Route exact path="/" component={UserPage} />
                <Route path={"/signin"} component={AuthPage} />
                <Route path="" component={NotFound} />
            </Switch>
        </BrowserRouter>
    );
}
