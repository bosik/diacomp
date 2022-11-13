import React, {PureComponent} from "react";
import "./InfoLines.css";

export class InfoLines extends PureComponent {
    constructor(props) {
        super(props);
    }

    render() {
        const children = this.props.data.map(e => (
            <div className="info-line">
                <div className="info-line-key">{e.key}</div>
                <div className={e.bold ? "info-line-value info-line-value-bold" : "info-line-value"}>{e.value}</div>
            </div>
        ));
        return (
            <div className="info-lines">
                {children}
            </div>
        );
    }
}

