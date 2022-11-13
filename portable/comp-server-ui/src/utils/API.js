import config from "../config";

export class AuthorizationError extends Error {}
export class NotFoundError extends Error {}
export class ServerError extends Error {}

export function getData({url, options = {}}) {
    let newOptions = Object.assign(options);
    newOptions.credentials = "include";

    return fetch(config.apiBaseUrl + url, newOptions)
        .catch(() => {
            throw new Error("Проблемы с сетью. Проверьте свое подключение")
        })
        .then(result => {
            switch (result.status) {
                case 401:
                    window.location = `/signin?from=${window.location.pathname}`;
                    throw new AuthorizationError();
                case 400:
                case 404:
                    throw new NotFoundError("Такой страницы не существует");
                case 500:
                    throw new ServerError("Ошибка сервера");
                default:
                // do nothing
            }

            return result
        })
        .then(res => res.json())
        .catch(err => {
            if (err instanceof AuthorizationError) {
                return new Promise(resolve => {
                    // Wait a bit to make redirect happen before promise resolution
                    setTimeout(() => resolve({}), 5000)
                });
            } else {
                throw err;
            }
        });
}


