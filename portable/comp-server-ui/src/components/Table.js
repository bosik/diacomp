import React, {PureComponent} from "react";
import "./Table.css";

export class Table extends PureComponent {

    state = {
        //selectedIndex: -1
    };

    constructor(props) {
        super(props);

        this.setState({selectedIndex: props.selectedIndex});
    }

    rows;

    render() {
        const {items, getKey, columnNames, columnStyles, getValues } = this.props;
        const { selectedIndex } = this.state;
        // const selectedIndex = this.state.selectedIndex || this.props.selectedIndex;
        // const selectedIndex = this.props.selectedIndex;


        if (items === null || items === undefined) {
            this.rows = <div className="table-info">Загрузка...</div>;
        } else if (items.length === 0) {
            this.rows = <div className="table-info">Ничего не найдено</div>;
        } else {
            // if (items.length > 50) {
            //     items = items.slice(0, 50);
            // }

            this.rows = [];
            for (let i = 0; i < items.length; i++) {
                const values = getValues(items[i]);
                let cells = [];
                for (let j = 0; j < values.length; j++) {
                    cells.push(<div className={`table-cell ${columnStyles[j]}`}>{values[j]}</div>);
                }

                this.rows.push(<div
                    key={getKey(items[i])}
                    className={selectedIndex === i ? "table-row table-row-selected" : "table-row"}
                    onClick={() => this.onRowSelected(i)}
                >
                    {cells}
                </div>);
            }
        }

        let headerCells = [];
        for (let i = 0; i < columnNames.length; i++) {
            headerCells.push(<div className={`table-cell ${columnStyles[i]}`}>{columnNames[i]}</div>);
        }

        return <div className="table">
            <div className="table-header">
                {headerCells}
            </div>
            <div className="table-content" tabIndex="0" onKeyDown={this.onKeyDown}>
                {this.rows}
            </div>
        </div>;
    }

    onRowSelected = (index) => {
        this.setState({selectedIndex: index});
        if (this.props.onRowSelected) {
            this.props.onRowSelected(index);
        }

        if (this.rows && index >= 0 && index < this.rows.length) {
            // this.rows[index].scrollIntoView();
        }
    };

    limit = (value, min, max) => {
        if (value < min) {
            value = min;
        }
        if (value > max) {
            value = max;
        }
        return value;
    };

    onKeyDown = (event) => {
        const selectedIndex = this.state.selectedIndex || 0;
        console.log(event.keyCode);

        switch (event.keyCode) {
            case 38: {
                // up
                const newIndex = this.limit(selectedIndex - 1, 0, this.props.items.length - 1);
                if (newIndex !== selectedIndex) {
                    this.onRowSelected(newIndex);
                }

                event.preventDefault();
                break;
            }

            case 40: {
                // down
                const newIndex = this.limit(selectedIndex + 1, 0, this.props.items.length - 1);
                if (newIndex !== selectedIndex) {
                    this.onRowSelected(newIndex);
                }

                event.preventDefault();
                break;
            }

            case 36: {
                // home
                const newIndex = 0;
                if (newIndex !== selectedIndex) {
                    this.onRowSelected(newIndex);
                }

                event.preventDefault();
                break;
            }

            case 35: {
                // end
                const newIndex = this.props.items.length - 1;
                if (newIndex !== selectedIndex) {
                    this.onRowSelected(newIndex);
                }

                event.preventDefault();
                break;
            }
        }
    };
}