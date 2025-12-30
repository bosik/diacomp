import React, {Component} from "react";
import "./Layout.css";

export class MainHeader extends Component {

    render() {
        return (<div className="block-header">
            <a href="/diary">
                <img className="logo" src="/img/logo.png" alt="" />
            </a>
            {this.props.title && <div className="block-title">{this.props.title}</div>}
            {this.props.children}
        </div>);
    }
}

export class MainContent extends Component {

    render() {
        return (<div className="block-content">
            {this.props.children}
        </div>);
    }
}
