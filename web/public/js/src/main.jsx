import React from 'react';
import ReactDOM from 'react-dom';
// import App from './components/App.jsx';
import { BrowserRouter as Router, Switch, Route } from 'react-router-dom'

import Root from "./components/pages/Root.jsx";
import Preview from "./components/pages/Preview.jsx";


ReactDOM.render(
<Router>
    <Switch>
        <Route exact path="/" component={Root}></Route>
        <Route path="/preview/:id" component={Preview}></Route>
    </Switch>

</Router>, document.getElementById('app'));