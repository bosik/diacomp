import React from "react";
import {BrowserRouter, Route, Switch} from "react-router-dom";
import {PageDiary} from "./pages/PageDiary";
import {getToken} from "./utils/authToken";
import {PageAuth} from "./pages/PageAuth";

export function MainRouting() {
    return (
        <BrowserRouter>
            <Switch>
                <Route path={"/diary"} component={getToken() ? PageDiary : PageAuth}/>
                {/*<Route path={""} component={}/>*/}
            </Switch>
        </BrowserRouter>
    );
}
