import React, {Component} from "react";
import "./PageAuth.css";
import {clearToken, setToken} from "../utils/authToken";
import config from "../config";
import {MainContent, MainHeader} from "../components/Layout";
// import TextField from "@material-ui/core/TextField";

export class PageAuth extends Component {

    constructor(props) {
        super(props);

        this.state = {
            username: "",
            password: "",
            loading: false,
            error: null
        };
    }

    onChangeUsername = (e) => {
        this.setState({username: e.target.value});
    };

    onChangePassword = (e) => {
        this.setState({password: e.target.value});
    };

    onClickSignIn = () => {
        this.setState(
            {loading: true},
            () => {
                fetch(config.backendUrl + '/auth/signin', {
                    method: "POST",
                    headers: {
                        "Content-Type": "application/json"
                    },
                    body: JSON.stringify({
                        username: this.state.username,
                        password: this.state.password
                    })
                })
                    .catch(() => {
                        throw new Error("Network issue");
                    })
                    .then(result => {
                        switch (result.status) {
                            case 401:
                                throw new Error("Invalid username / password");
                            default:
                                return result;
                        }
                    })
                    .then(res => res.json())
                    .then(token => {
                        setToken(`${token.tokenType} ${token.accessToken}`);
                        this.setState({loading: false, error: null});
                        window.location.reload();
                    })
                    .catch(error => {
                        clearToken();
                        this.setState({loading: false, error: error.message});
                    });
            }
        );
    };

    render() {
        return [<MainHeader title="Авторизация" />,
            <MainContent>
                <form method="post" className="card">
                    <div className="form-group">
                        <input id="fieldUserName" type="text" className="lowercase" autoFocus />
                        <label className="outline-label" for="fieldUserName">Email</label>
                    </div>
                    <div className="form-group">
                        <input id="fieldPassword" type="password" className="" />
                        <label className="outline-label" for="fieldPassword">Пароль</label>
                    </div>
                    <input type="hidden" name="api" value="20" />
                    <button id="buttonLogin" type="submit" className="btn btn-primary full-width">Войти</button>
                    <div>Ещё нет учётной записи? <a href="/signup">Создать</a></div>
					<div>Забыли пароль? <a href="/restore">Восстановить</a></div>
				</form>
            </MainContent>];
    }
}
