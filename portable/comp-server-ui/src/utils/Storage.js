class Entry {
    key;
    defaultValue;
    reader;
    writer;

    constructor(key, defaultValue, reader = e => e, writer = e => e) {
        this.key = key;
        this.defaultValue = defaultValue;
        this.reader = reader;
        this.writer = writer;
    }

    get() {
        const value = localStorage.getItem(this.key);
        return value
            ? this.reader(value)
            : this.defaultValue;
    }

    set(value) {
        localStorage.setItem(this.key, this.writer(value));
    }
}

export class Storage {
    static ActiveTab = new Entry("diacomp_activeTab", 0);
    static SelectedDate = new Entry("diacomp_diary_date", new Date(), value => new Date(value));
    static SelectedDiaryRecord = new Entry("diacomp_diary_selectedRecord");
    static BasesSearchText = new Entry("diacomp_bases_search", "");
    static BasesFoodSelected = new Entry("diacomp_bases_food_selected", -1, value => parseInt(value));
    static BasesDishSelected = new Entry("diacomp_bases_dish_selected", -1, value => parseInt(value));
}