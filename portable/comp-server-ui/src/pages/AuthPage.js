import React, {PureComponent} from "react";
import "./AuthPage.css";
import {getSearchParams} from "../utils/getSearchParams";
import config from "../config";

export class AuthPage extends PureComponent {
    constructor(props) {
        super(props);
        this.state = {
            username: "",
            password: "",
            error: ""
        };
    }

    onClickLogin = () => {
        const requestParams = {
            "username": this.state.username,
            "password": this.state.password
        };

        fetch(config.apiBaseUrl + "/auth/login/json", {
            method: "POST",
            credentials: "include",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify(requestParams),
        })
            .then(result => {
                if (result.status === 200) {
                    const searchParams = getSearchParams();

                    if (searchParams.from) {
                        window.location = searchParams.from;
                    } else {
                        window.location = "/";
                    }
                } else if (result.status === 400 || result.status === 401) {
                    this.setState({
                        error: "Неверный логин и/или пароль"
                    });
                } else {
                    this.setState({
                        error: "Что-то пошло не так, обратитесь к администратору"
                    });
                }
            })
            .catch(() => {
                this.setState({
                    error: "У нас что-то сломалось, пожалуйста, попробуйте авторизоваться позже"
                });
            });
    };

    onChangeUsername = (e) => {
        this.setState({username: e.target.value});
    };

    onChangePassword = (e) => {
        this.setState({password: e.target.value});
    };

    render() {
        return (
            <div className="panel">
                <input className="big-input" placeholder="Логин (email)" value={this.state.username}
                       onChange={this.onChangeUsername}/>
                <input className="big-input" placeholder="Пароль" type="password" value={this.state.password}
                       onChange={this.onChangePassword}/>
                <button className="big-input" onClick={this.onClickLogin}>Войти</button>
                {this.state.error && <div>Ошибка авторизации: {this.state.error}</div>}
            </div>
        );
    }
}
