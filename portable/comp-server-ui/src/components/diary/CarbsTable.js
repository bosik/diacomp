import React, {PureComponent} from "react";
import "./CarbsTable.css";

export class CarbsTable extends PureComponent {
    constructor(props) {
        super(props);
    }

    render() {
        const {carbs, bsIn, bsTarget, k, q, selectedDose, maxDose} = this.props;
        const bsDelta = bsTarget - bsIn;

        const rows = [];
        for (let dose = 0; dose <= maxDose; dose++) {
            const correction = (dose * q + bsDelta) / k - carbs;
            const total = (bsDelta + dose * q) / k;

            rows.push(<div
                    className={dose === selectedDose ? "carbs-table-row cars-table-row-selected" : "carbs-table-row"}>
                    <div className="carbs-table-cell">{dose} ед.</div>
                    <div className="carbs-table-cell">{(correction > 0 ? "+" : "") + correction.toFixed(0)} г</div>
                    <div className="carbs-table-cell">{total.toFixed(0)} г</div>
                </div>
            );
        }

        return (
            <div className="carbs-table">
                <div className="carbs-table-header">
                    <div className="carbs-table-cell">Доза</div>
                    <div className="carbs-table-cell">Коррекция</div>
                    <div className="carbs-table-cell">Итого</div>
                </div>
                {rows}
            </div>
        );
    }
}

