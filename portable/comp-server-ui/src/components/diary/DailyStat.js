import React, {PureComponent} from "react";
import "./DailyStat.css";
import {InfoSection} from "../InfoSection";
import {InfoLines} from "../InfoLines";

export class DailyStat extends PureComponent {

    render() {
        const {prots, fats, carbs, value, mass, insulin}  = this.props.data;

        return (
            <InfoSection caption="Статистика">
                <InfoLines data={[
                    {"key": "Белки:", "value": prots.toFixed(0) + " г"},
                    {"key": "Жиры:", "value": fats.toFixed(0) + " г"},
                    {"key": "Угл.:", "value": carbs.toFixed(0) + " г"},
                    {"key": "Ценность:", "value": value.toFixed(0) + " ккал"},
                    {"key": "Масса:", "value": mass.toFixed(0) + " г"},
                    {"key": "Инсулин:", "value": insulin.toFixed(1) + " ед"}
                ]}/>
            </InfoSection>
        );
    }
}