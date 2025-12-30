const TOKEN_KEY = "diacomp_token";

export function getToken() {
    return null; // localStorage.getItem(TOKEN_KEY);
}

export function getTokenValue() {
    let token = getToken();
    if (token && token.length > 10) {
        token = token.substring(7);
    }
    return token;
}

export function setToken(value) {
    if (value) {
        localStorage.setItem(TOKEN_KEY, value);
    }
}

export function clearToken() {
    localStorage.removeItem(TOKEN_KEY);
}
