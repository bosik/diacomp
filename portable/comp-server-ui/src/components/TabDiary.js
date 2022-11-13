import React, {PureComponent} from "react";
import "./TabDiary.css";
import {InfoSection} from "./InfoSection";
import {InfoLines} from "./InfoLines";
import {CarbsTable} from "./diary/CarbsTable";
import Calendar from "react-calendar";
import "react-calendar/dist/Calendar.css";
import {getData} from "../utils/API";
import {DiaryBlood} from "./diary/record/DiaryBlood";
import {DailyStat} from "./diary/DailyStat";
import {convertToStringUTC, resetTime, shiftDate} from "../utils/DateUtils";
import {DiaryIns} from "./diary/record/DiaryIns";
import {Storage} from "../utils/Storage";
import {DiaryMeal} from "./diary/record/DiaryMeal";

export class TabDiary extends PureComponent {

    state = {
        error: null,
        isLoaded: false,
        date: null,
        data: [],
        selectedDiaryRecord: Storage.SelectedDiaryRecord.get()
    };

    constructor(props) {
        super(props);
    }

    componentDidMount() {
        this.loadDiary(Storage.SelectedDate.get());
    }

    loadDiary(date) {
        date = resetTime(date);
        const startTime = convertToStringUTC(date);
        const endTime = convertToStringUTC(shiftDate(date, +1));

        getData({
            url: `/diary/period?start_time=${startTime}&end_time=${endTime}`,
        })
            .then(result => {
                this.setState({
                    date,
                    isLoaded: true,
                    data: result,
                })
            })
            .catch(error => {
                this.setState({
                    date,
                    isLoaded: true,
                    data: [],
                    error
                })
            })
    }

    render() {
        const {error, isLoaded, data} = this.state;

        if (error) {
            return (
                <div>
                    {error.message}
                </div>
            );
        }

        if (!isLoaded) {
            return (
                <div className="spinner-wrapper">
                    {/*<Spinner*/}
                    {/*animation="border"*/}
                    {/*role="status"*/}
                    {/*variant="secondary"*/}
                    {/*>*/}
                    <span className="sr-only">Загрузка...</span>
                    {/*</Spinner>*/}
                </div>
            );
        }

        return (
            <div>
                <div id="panel-left">
                    <Calendar onChange={this.onCalendarChanged} value={this.state.date}/>
                    <DailyStat data={this.getDailyStat(data)}/>
                    <InfoSection id="diary-daily-chart" caption="График СК">
                        Chart here
                    </InfoSection>
                    <InfoSection caption="Дополнительно">
                        <InfoLines id="diary-misc" data={[
                            {"key": "После укола:", "value": "04:09", "bold": true},
                            {"key": "После еды:", "value": "05:53", "bold": true},
                            {"key": "Палец:", "value": "< средний", "bold": true}
                        ]}/>
                    </InfoSection>
                </div>
                <div id="panel-right">
                    <InfoSection id="diary-meal-stat" caption="Приём пищи">
                        <InfoLines id="diary-misc" data={[
                            {"key": "Белки:", "value": "30 г"},
                            {"key": "Жиры:", "value": "36 г"},
                            {"key": "Угл.:", "value": "89 г / 7.4 ХЕ"},
                            {"key": "Ценность:", "value": "803 ккал"},
                            {"key": "Масса:", "value": "653 г"}
                        ]}/>
                    </InfoSection>
                    <InfoSection id="diary-meal-stat" caption="Инсулин">
                        <InfoLines id="diary-misc" data={[
                            {"key": "Расчётная доза:", "value": "11 ед", "bold": true},
                            {"key": "Коррекция:", "value": "+0.8 ммоль/л"},
                            {"key": "Ожидаемый СК:", "value": "5.0 ммоль/л"}
                        ]}/>
                    </InfoSection>
                    <InfoSection id="diary-meal-stat" caption="Подбор углеводов">
                        <CarbsTable carbs={89} bsIn={4.2} bsTarget={5.0} k={0.31} q={2.4} selectedDose={11}
                                    maxDose={22}/>
                    </InfoSection>
                </div>
                <div id="panel-center" onClick={e => this.onRecordSelected(e, null)}>
                    <div className="diary">
                        {data.map(e => this.buildDiaryRecord(e))}
                    </div>
                    <div id="diary-add">
                        <div className="button-add">Blood</div>
                        <div className="button-add">Ins</div>
                        <div className="button-add">Meal</div>
                        <div className="button-add">Note</div>
                    </div>
                </div>
            </div>
        );
    }

    buildDiaryRecord(record) {
        const selectedId = this.state.selectedDiaryRecord;
        switch (record.data.type) {
            case "blood":
                return <DiaryBlood data={record}
                                   key={record.id}
                                   selected={selectedId === record.id}
                                   onClick={(e) => this.onRecordSelected(e, record)}/>;
            case "ins":
                return <DiaryIns data={record}
                                 key={record.id}
                                 selected={selectedId === record.id}
                                 onClick={(e) => this.onRecordSelected(e, record)}/>;
            case "meal":
                return <DiaryMeal data={record}
                                  key={record.id}
                                  selected={selectedId === record.id}
                                  onClick={(e) => this.onRecordSelected(e, record)}/>;
            case "note":
                return <div className="diary-record">{record.data.time} - note</div>;
        }

    };

    getDailyStat(data) {
        let result = {
            prots: 0,
            fats: 0,
            carbs: 0,
            value: 0,
            mass: 0,
            insulin: 0
        };

        data.forEach(e => {
            switch (e.data.type) {
                case "ins" : {
                    result.insulin += e.data.value;
                    break;
                }

                case "meal" : {
                    e.data.content.forEach(food => {
                        result.prots += food.prots * food.mass / 100;
                        result.fats += food.fats * food.mass / 100;
                        result.carbs += food.carbs * food.mass / 100;
                        result.value += food.value * food.mass / 100;
                        result.mass += food.mass;
                    });

                    break;
                }
            }
        });

        return result;
    }

    onCalendarChanged = (newDate) => {
        Storage.SelectedDate.set(newDate);
        this.loadDiary(newDate);
    };

    onRecordSelected = (event, record) => {
        const id = record ? record.id : null;
        Storage.SelectedDiaryRecord.set(id);
        this.setState({selectedDiaryRecord: id});
        event.stopPropagation();
    };
}

