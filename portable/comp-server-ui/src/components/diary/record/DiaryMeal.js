import React, {PureComponent} from "react";
import "./DiaryMeal.css";
import {DiaryRecord} from "./DiaryRecord";

export class DiaryMeal extends PureComponent {

    render() {
        return (
            <DiaryRecord
                className={this.props.selected ? "diary-record-meal-selected" : "diary-record-meal"}
                data={this.props.data}
                onClick={this.props.onClick}>
                <div>{this.formatValue(this.props.data.data)}</div>
            </DiaryRecord>
        );
    }

    formatValue = (data) => {
        const foodList = data.content.map(food => <div className="food-item">
            <div className="food-item-name"><input className="transparent input-name" value={food.name}/></div>
            <div className="food-item-mass"><input className="transparent input-mass" value={food.mass}/></div>
        </div>);
        return <div className="food-list">{foodList}</div>
    }
}