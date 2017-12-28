import React, { Component } from "react";
import ConfigsList from "./ConfigsList.jsx";
import ConfigFileInput from "./ConfigFileInput.jsx";

export default class Config extends Component {
    componentWillMount() {
        this.setState({
            configs: [],
        });
    }
    onAdd(cnf) {
        this.setState({
            configs: [ ...this.state.configs, { filename: 'heeey.hs' } ]
        })
    }
    render() {
        return <div>
                <b>Configurations:</b>
                <ConfigsList configs={this.state.configs} />
                <ConfigFileInput onAdd={this.onAdd.bind(this)} />
            </div>
    }
}