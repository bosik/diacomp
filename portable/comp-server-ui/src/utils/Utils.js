import config from "../config";
import {clearToken, getToken} from "./authToken";

export function requestApi(url, options = {}) {
    const token = getToken();
    let newOptions = Object.assign(options);

    if (token) {
        newOptions.headers = newOptions.headers || {};
        newOptions.headers["Authorization"] = token;
    }

    return fetch(config.backendUrl + url, newOptions)
        .catch(() => {
            throw new Error("Network issue");
        })
        .then(result => {
            switch (result.status) {
                case 401:
                    clearToken();
                    window.location.reload();
                    break;
                default:
                    // ignore
                //throw new AuthorizationError();
            }

            return result;
        });
}

export function requestApiJson(url, options = {}) {
    return requestApi(url, options)
        .then(res => res.json());
}

export function requestApiRaw(url, options = {}) {
    return requestApi(url, options)
        .then(res => res.blob());
}

export function formatFileSize(input) {
    if (isNaN(input)) return input;

    let units = ["bytes", "KB", "MB", "GB"];
    let size = parseInt(input);
    let i = 0;
    while (i < units.length - 1 && size > 1024) {
        i++;
        size /= 1024;
    }

    return size.toFixed(0) + " " + units[i];
}
