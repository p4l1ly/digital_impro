import React, { Component } from 'react';
import SocketIOFileClient from "socket.io-file-client";

export default class ConfigFileInput extends Component {
    handleSubmit(e) {
        e.preventDefault();
        this.props.onAdd(this.fileInput);
    }
    render() {
        return <form onSubmit={this.handleSubmit.bind(this)} datatype="multipart/form-data">
                <label>Select config file: </label>
                <input type="file" id="config-file-input" ref={fileInput => this.fileInput = fileInput} />
                <button type="submit">Add</button>
            </form>
    }
}