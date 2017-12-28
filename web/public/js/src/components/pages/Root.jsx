import React, { Component } from "react";
import Config from "../root/Config.jsx";

export default class Root extends Component {
    render() {
        return <div>
                <h1>Hello</h1>
                <Config />
            </div>
    }
}