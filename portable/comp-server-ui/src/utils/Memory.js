import {getData} from "./API";
export class Memory {
    static foodBase;
    static dishBase;

    static loadFoodBase(onOk, onError) {
        if (this.foodBase) {
            onOk(this.foodBase);
        } else {
            getData({
                url: `/food/all`,
            })
                .then(result => {
                    this.foodBase = result;
                    onOk(result);
                })
                .catch(error => onError(error));
        }
    }

    static loadDishBase(onOk, onError) {
        if (this.dishBase) {
            onOk(this.dishBase);
        } else {
            getData({
                url: `/dish/all`,
            })
                .then(result => {
                    this.dishBase = result;
                    onOk(result);
                })
                .catch(error => onError(error));
        }
    }
}