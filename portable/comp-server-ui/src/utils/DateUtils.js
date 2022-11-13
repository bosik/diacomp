export function format00(n) {
    return (n < 10 ? "0" : "") + n;
}

export function convertToStringUTC(date) {
    return date.getUTCFullYear() + "-" +
        format00(date.getUTCMonth() + 1) + "-" +
        format00(date.getUTCDate()) + " " +
        format00(date.getUTCHours()) + ":" +
        format00(date.getUTCMinutes()) + ":" +
        format00(date.getUTCSeconds());
}

export function formatLocalDate(date) {
    return date.getFullYear() + "-" +
        format00(date.getMonth() + 1) + "-" +
        format00(date.getDate());
}

export function resetTime(date) {
    return new Date(
        date.getFullYear(),
        date.getMonth(),
        date.getDate(),
    );
}

export function shiftDate(date, days) {
    let result = new Date(date);
    result.setDate(result.getDate() + days);
    return result;
}

export function parseDateTimeUTC(s) {
    return new Date(s + " UTC");
}
