import React, {PureComponent} from "react";
import "./DiaryIns.css";
import {DiaryRecord} from "./DiaryRecord";

export class DiaryIns extends PureComponent {

    render() {
        return (
            <DiaryRecord
                className={this.props.selected ? "diary-record-ins-selected" : "diary-record-ins"}
                data={this.props.data}
                onClick={this.props.onClick}>
                <div>{this.formatValue(this.props.data.data)}</div>
            </DiaryRecord>
        );
    }

    formatValue = (data) => {
        return data.value.toFixed(1) + " ед";
    }
}