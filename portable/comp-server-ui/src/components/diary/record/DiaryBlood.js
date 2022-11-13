import React, {PureComponent} from "react";
import "./DiaryBlood.css";
import {DiaryRecord} from "./DiaryRecord";

export class DiaryBlood extends PureComponent {

    render() {
        return (
            <DiaryRecord
                className={this.props.selected ? "diary-record-blood-selected" : "diary-record-blood"}
                data={this.props.data}
                onClick={this.props.onClick}>
                <div>{this.formatValue(this.props.data.data)}</div>
            </DiaryRecord>
        );
    }

    formatValue = (data) => {
        const {value, finger} = data;
        const FINGERS = ["БЛ", "1Л", "2Л", "3Л", "4Л", "4П", "3П", "2П", "1П", "БП"];
        return value.toFixed(1) + " ммоль/л" + (finger !== undefined ? " (" + FINGERS[finger] + ")" : "");
    }
}