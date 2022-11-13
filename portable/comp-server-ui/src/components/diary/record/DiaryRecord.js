import React, {PureComponent} from "react";
import "./DiaryRecord.css";

export class DiaryRecord extends PureComponent {

    render() {
        return (
            <div className={`diary-record ${this.props.className}`} onClick={this.props.onClick}>
                <div className="record-time">{this.formatTime(this.props.data.data.time)}</div>
                <div className="record-value">{this.props.children}</div>
                <a className="button-remove" title="Удалить" href="#">X</a>
            </div>
        );
    }

    formatTime = (time) => {
        return time && new Date(time + " UTC")
                .toLocaleTimeString(
                    'en-US',
                    {hourCycle: "h23", hour: "2-digit", minute: "2-digit"}
                );
    };
}