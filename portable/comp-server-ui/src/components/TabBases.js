import React, {PureComponent} from "react";
import "./TabBases.css";
import {formatLocalDate, parseDateTimeUTC} from "../utils/DateUtils";
import {Storage} from "../utils/Storage";
import {loadFoodBase, Memory} from "../utils/Memory";
import {Table} from "./Table";

export class TabBases extends PureComponent {

    state = {
        error: null,
        searchText: Storage.BasesSearchText.get(),
        foods: null,
        dishes: null
    };

    componentDidMount() {
        this.loadFoodBase();
        this.loadDishBase();
    }

    loadFoodBase() {
        Memory.loadFoodBase(
            result => {
                this.setState({
                    foods: result,
                });
            },
            error => {
                this.setState({
                    foods: [],
                    error
                });
            }
        );
    }

    loadDishBase() {
        Memory.loadDishBase(
            result => {
                this.setState({
                    dishes: result,
                });
            },
            error => {
                this.setState({
                    dishes: [],
                    error
                });
            }
        );
    }

    render() {
        return (
            <div className="tab-bases">
                <div className="base-search">
                    <input placeholder="Поиск..." value={this.state.searchText} onChange={this.onChangeSearch}/>
                </div>
                <div className="base-panels">
                    {this.getFoodPanel()}
                    {this.getDishPanel()}
                </div>
            </div>
        );
    }

    matches = (item, search) => {
        return !search || (item && item.toLowerCase().includes(search.toLowerCase()));
    };

    getFoodPanel = () => {
        const {error, foods} = this.state;

        // if (error) {
        //     items = <div className="table-info">{error.message}</div>;
        // } else
        const filtered = foods !== null
            ? foods.filter(e => this.matches(e.data.name, this.state.searchText))
            : foods;

        return <div className="base-panel">
            <div className="base-header">База продуктов {filtered !== null ? `(${filtered.length})` : ""}</div>
            <Table
                items={filtered}
                getKey={e => e.id}
                columnNames={[
                    "Наименование",
                    "Б",
                    "Ж",
                    "У",
                    "ккал",
                    "Дата"
                ]}
                columnStyles={[
                    "col-name",
                    "col-value",
                    "col-value",
                    "col-value",
                    "col-value",
                    "col-date"
                ]}
                getValues={e => [
                    e.data.name,
                    e.data.prots.toFixed(1),
                    e.data.fats.toFixed(1),
                    e.data.carbs.toFixed(1),
                    e.data.value.toFixed(0),
                    formatLocalDate(parseDateTimeUTC(e.stamp))
                ]}
                selectedIndex={Storage.BasesFoodSelected.get()}
                onRowSelected={this.onFoodSelected}
            />
        </div>;
    };

    getDishPanel = () => {
        const {error, dishes} = this.state;

        // if (error) {
        //     items = <div className="table-info">{error.message}</div>;
        // } else
        const filtered = dishes !== null
            ? dishes.filter(e => this.matches(e.data.name, this.state.searchText))
            : dishes;

        return <div className="base-panel">
            <div className="base-header">База блюд {filtered !== null ? `(${filtered.length})` : ""}</div>
            <Table
                items={filtered}
                getKey={e => e.id}
                columnNames={[
                    "Наименование",
                    "Масса",
                    "Б",
                    "Ж",
                    "У",
                    "ккал",
                    "Дата"
                ]}
                columnStyles={[
                    "col-name",
                    "col-value",
                    "col-value",
                    "col-value",
                    "col-value",
                    "col-value",
                    "col-date"
                ]}
                getValues={e => [
                    e.data.name,
                    e.data.realMass.toFixed(0),
                    this.getRelDish(e.data, item => item.prots).toFixed(1),
                    this.getRelDish(e.data, item => item.fats).toFixed(1),
                    this.getRelDish(e.data, item => item.carbs).toFixed(1),
                    this.getRelDish(e.data, item => item.value).toFixed(0),
                    formatLocalDate(parseDateTimeUTC(e.stamp))
                ]}
                selectedIndex={Storage.BasesDishSelected.get()}
                onRowSelected={this.onDishSelected}
            />
        </div>;
    };

    getRelDish = (data, prop) => {
        if (data.realMass) {
            return data.content
                    .map(item => prop(item) * item.mass / 100)
                    .reduce((a, b) => a + b, 0)
                * 100 / data.realMass;
        } else {
            return 0;
        }
    };

    onChangeSearch = (e) => {
        Storage.BasesSearchText.set(e.target.value);
        this.setState({searchText: e.target.value});
        this.onFoodSelected(-1);
        this.onDishSelected(-1);
    };

    onFoodSelected = (index) => {
        Storage.BasesFoodSelected.set(index);
    };

    onDishSelected = (index) => {
        Storage.BasesDishSelected.set(index);
    };
}

