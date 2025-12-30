import React, {Component} from "react";
import "./PageDiary.css";
import {getTokenValue} from "../utils/authToken";
import {formatFileSize, requestApiJson, requestApiRaw} from "../utils/Utils";
import config from "../config";
import {MainContent, MainHeader} from "../components/Layout";
import Navbar from "react-bootstrap/Navbar";
import Offcanvas from "react-bootstrap/Offcanvas";
import Container from "react-bootstrap/Container";
import {Nav} from "react-bootstrap";

export class PageDiary extends Component {

    constructor(props) {
        super(props);

        this.state = {
            loading: true
        };

        this.resourceId = this.props.match.params.resourceId;
    }

    componentDidMount() {
        this.setState({loading: true},
            () => {
                requestApiJson(`/res/${encodeURIComponent(this.resourceId)}/meta?token=${getTokenValue()}`, {
                    method: "GET",
                    headers: {
                        "Content-Type": "application/json"
                    }
                })
                    .then(response => {
                        response.properties = this.convertToList(response.properties);
                        response.properties.sort((a, b) => a.key.localeCompare(b.key, "en"));

                        this.setState(state => {
                            state.meta = response;
                            state.loading = false;
                            return state;
                        });
                    });
            });
    }

    render() {
        return [<MainHeader title={this.state.meta?.resource.fileName}>
            <Navbar expand={false} data-bs-theme="dark">
                <Container fluid>
                    <Navbar.Toggle aria-controls={`offcanvasNavbar-expand`} className="menu-icon"/>
                    <Navbar.Offcanvas
                        aria-labelledby={`offcanvasNavbarLabel-expand`}
                        placement="end"
                    >
                        <Offcanvas.Body>
                            <Nav className="justify-content-end flex-grow-1">
                                <Nav.Link className="nav-link" onClick={this.download}>
                                    <img src="/img/ic-download.png" className="action-icon" alt=""/>
                                    Download
                                </Nav.Link>
                                <Nav.Link className="nav-link">
                                    <img src="/img/ic-rename.png" className="action-icon" alt=""/>
                                    Rename...
                                </Nav.Link>
                                <Nav.Link className="nav-link">
                                    <img src="/img/ic-move.png" className="action-icon" alt=""/>
                                    Move...
                                </Nav.Link>
                                <Nav.Link className="nav-link danger">
                                    <img src="/img/ic-delete.png" className="action-icon" alt=""/>
                                    Delete
                                </Nav.Link>
                            </Nav>
                        </Offcanvas.Body>
                    </Navbar.Offcanvas>
                </Container>
            </Navbar>
        </MainHeader>,
            <MainContent>
                {this.renderContent()}
            </MainContent>
        ];
    }

    renderContent = () => {
        if (this.state.loading) {
            return <div className="pl-3 pr-3 pt-3 pb-3">Loading...</div>;
        }

        return <div className="container">
            {this.renderMedia()}
            {this.renderCommon()}
        </div>;
    };

    renderMedia = () => {
        if (this.state.meta.resource.contentType.startsWith("image/")) {
            return <img src={this.resourceUrl()} className="large-image" alt={this.state.meta?.resource.fileName}/>;
        }

        if (this.state.meta.resource.contentType.startsWith("audio/")) {
            return <audio controls autoPlay>
                <source src={this.resourceUrl()} type={this.state.meta.resource.contentType}/>
                Your browser does not support the audio tag
            </audio>;
        }

        if (this.state.meta.resource.contentType.startsWith("video/")) {
            return <video width="100%" controls autoPlay>
                <source src={this.resourceUrl()} type={this.state.meta.resource.contentType}/>
                Your browser does not support the video tag
            </video>;
        }

        return null;
    };

    download = () => {
        requestApiRaw("/res/" + this.resourceId + "?token=" + getTokenValue(), {
            method: "GET"
        })
            .then(blob => {
                const link = document.createElement("a");
                link.download = this.state.meta.resource.fileName;
                link.href = URL.createObjectURL(blob);
                link.click();

                URL.revokeObjectURL(link.href);
            })
            .catch(() => {
                // TODO
            });
    };

    renderCommon = () => {
        return [
            <div>
                <div className="pl-2 pr-2 mt-2 section">File</div>
                <div className="pl-2 pr-2">
                    <a href={config.backendUrl + "/res/" + this.resourceId + "?token=" + getTokenValue()}
                       target="_blank" rel="noreferrer">{this.state.meta.resource.fileName}</a>
                </div>
            </div>,
            <div>
                <div className="pl-2 pr-2 section">Location</div>
                <div className="pl-2 pr-2"><a href={"/browse?path=" + this.state.meta.resource.folder}>{this.state.meta.resource.folder}</a></div>
            </div>,
            <div>
                <div className="pl-2 pr-2 section">Size</div>
                <div className="pl-2 pr-2">{formatFileSize(this.state.meta.resource.contentSize)}</div>
            </div>,
            <div>
                <div className="pl-2 pr-2 section">Type</div>
                <div className="pl-2 pr-2">{this.state.meta.resource.contentType}</div>
            </div>,
            <div>
                <div className="pl-2 pr-2 section">Uploaded</div>
                <div className="pl-2 pr-2">{this.state.meta.resource.timeUploaded}</div>
            </div>,
            <div>
                <div className="pl-2 pr-2 section">Tags</div>
                <div className="pl-2 pr-2">
                    {this.state.meta.tags?.length > 0
                        ? this.state.meta.tags.map(t => <div>{t}</div>)
                        : <div className="muted">No tags</div>}
                </div>
            </div>,
            <div className="pb-3">
                <div className="pl-2 pr-2 section">Properties</div>
                {this.state.meta.properties.map(e =>
                    <div className="pl-2 pr-2 properties-row" key={e.key}>
                        <span className="properties-key pt-1">{e.key}: </span>
                        <span className="properties-value pt-1">{e.value}</span>
                    </div>)}
            </div>];
    };


    resourceUrl = () => {
        return config.backendUrl + "/res/" + this.resourceId + "?token=" + getTokenValue();
    };

    convertToList = (object) => {
        let list = [];
        Object.entries(object).forEach(([key, value]) => list.push({key, value}));
        return list;
    }
}
