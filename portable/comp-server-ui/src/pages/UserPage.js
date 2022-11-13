import React, {PureComponent} from "react";
import "./UserPage.css";
import {Tab, TabList, TabPanel, Tabs} from "react-tabs";
import "react-tabs/style/react-tabs.css";
import {TabDiary} from "../components/TabDiary";
import {Storage} from "../utils/Storage";
import {TabBases} from "../components/TabBases";
import config from "../config";

export class UserPage extends PureComponent {
    constructor(props) {
        super(props);
    }

    render() {
        return (
            <div className="App">
                <a href="#" className="button-logout" onClick={this.onClickLogout} tabIndex="-1">Выйти</a>
                <Tabs defaultIndex={Storage.ActiveTab.get()} onSelect={(index) => Storage.ActiveTab.set(index)}>
                    <TabList id="tabs">
                        <Tab>Дневник</Tab>
                        <Tab>Базы</Tab>
                        <Tab>Анализ</Tab>
                    </TabList>
                    <TabPanel>
                        <TabDiary />
                    </TabPanel>
                    <TabPanel>
                        <TabBases />
                    </TabPanel>
                    <TabPanel>
                        Content 3
                    </TabPanel>
                </Tabs>
            </div>
        );
    }

    onClickLogout = () => {
        fetch(config.apiBaseUrl + "/auth/logout", {
            credentials: "include"
        })
            .then(result => {
                window.location = "/";
            })
            .catch(() => {
                this.setState({
                    error: "У нас что-то сломалось, пожалуйста, попробуйте позже"
                });
            });
    };
}
