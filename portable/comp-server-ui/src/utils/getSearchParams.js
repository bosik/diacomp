export function getSearchParams() {
    const search = window.location.search;

    if (!search) {
        return {}
    }

    return search
        .slice(1)
        .split("&")
        .reduce((result, item) => {
            if (item) {
                const parts = item.split("=");
                const name = parts[0];
                const value = parts.slice(1).join("=");
                result[name] = value;
            }

            return result;
        }, {});
}
