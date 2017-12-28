import React, { Component } from 'react';

export default class ConfigsList extends Component {
    render() {
        const cfgs = this.props.configs.map((cfg, i) => {
            return <div key={'config-' + i} className="config-file">
                    <b>{cfg.filename}</b>
                    <label for={'config-' + i}>Select this one</label>
                    <input type="radio" name="config-file" id={'config-' + i} />
                </div>
            
        });
        return <div className="configs-list">
                {cfgs}
            </div>
    }
}