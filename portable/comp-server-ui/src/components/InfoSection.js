import React, {PureComponent} from "react";
import "./InfoSection.css";

export class InfoSection extends PureComponent {
    constructor(props) {
        super(props);
    }

    render() {
        return (
            <div className="info-section">
                <div className="info-section-title">{this.props.caption}</div>
                {this.props.children}
            </div>
        );
    }
}

