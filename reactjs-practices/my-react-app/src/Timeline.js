import React, { Component } from "react";

import classes from "./Timeline.css";

function findMatchingTimeUnits(event, timeUnits) {
    var includesBothUnits = timeUnits.filter(
        t => event.startTime.getTime() >= t.startTime.getTime() && event.endTime.getTime() <= t.endTime.getTime()
    );
    var includesStartTimeUnits = timeUnits.filter(
        t =>
            event.startTime.getTime() >= t.startTime.getTime() &&
            event.endTime.getTime() >= t.endTime.getTime() &&
            event.startTime.getTime() <= t.endTime.getTime()
    );
    var includesEndTimeUnits = timeUnits.filter(
        t =>
            event.startTime.getTime() <= t.startTime.getTime() &&
            event.endTime.getTime() <= t.endTime.getTime() &&
            event.endTime.getTime() >= t.startTime.getTime()
    );

    var spreadAcrossUnits = timeUnits.filter(
        t => event.startTime.getTime() <= t.startTime.getTime() && event.endTime.getTime() >= t.endTime.getTime()
    );

    var results = includesStartTimeUnits
        .concat(includesBothUnits)
        .concat(spreadAcrossUnits)
        .concat(includesEndTimeUnits);
    results = removeDups(results);
    return results;
}

function removeDups(a) {
    var seen = new Map();
    a.forEach(item => {
        seen.set(item.id, item);
    });

    var res = [];
    var it = seen.values();
    while (true) {
        var val = it.next();
        if (val.done) {
            break;
        }
        res.push(val.value);
    }

    return res;
}

// This component represents a time unit which can be a minute or hour.
// It will strike a bar for each event passed to it.
class Timebar extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            totalTimeUnits: props.timeUnit == "MINUTES" ? 24 * 60 : 24,
            hours: []
        };
    }

    componentDidMount() {
        var st = new Date(this.props.data.startTime);
        var hours = [];
        for (var i = 0; i < this.state.totalTimeUnits; i++) {
            var et = new Date(st);
            if (this.props.timeUnit == "MINUTES") {
                et.setMinutes(st.getMinutes() + 1);
            } else {
                et.setHours(st.getHours() + 1);
            }
            var hour = {
                id: i,
                startTime: st,
                endTime: et,
                events: []
            };
            hours.push(hour);
            st = et;
        }
        var events = this.props.data.events;
        events.forEach(e => {
            var matchingHours = findMatchingTimeUnits(e, hours);
            matchingHours.forEach(h => {
                h.class = e.class == "green" ? classes.Green : classes.Red;
                h.events.push(e);
            });
        });
        var firstConnectionStartTime = this.props.data.connectionStartTime;
        if (firstConnectionStartTime.getTime() >= this.props.data.endTime.getTime()) {
            firstConnectionStartTime = this.props.data.endTime;
        }

        if (firstConnectionStartTime.getTime() > this.props.data.startTime.getTime()) {
            var greyEvent = {
                startTime: this.props.data.startTime,
                endTime: firstConnectionStartTime,
                class: "grey",
                title: "Last Claimed at " + this.props.data.connectionStartTime.toLocaleString()
            };
            var greyUnits = findMatchingTimeUnits(greyEvent, hours);
            greyUnits.forEach(g => {
                g.events = [greyEvent];
                g.class = classes.Grey;
                g.title = greyEvent.title;
            });
        }
        this.setState({
            hours: hours
        });
    }

    prepareToolTip(hour) {
        if (hour.events.length == 0) {
            return "";
        }

        // Since multiple events may overlap in an hour, the hour object will store all the events.
        // But when comes to tooltip, the last event would be sufficient.
        var e = hour.events[hour.events.length - 1];
        if (e.class != undefined && e.class.toLowerCase() == "grey") {
            return e.title;
        }
        var eventName;
        if (e.class == undefined) {
            eventName = "Disconnection Event";
        } else if (e.class.toLowerCase() == "red") {
            eventName = "Disconnection Event";
        } else if (e.class.toLowerCase() == "green") {
            eventName = "Connection Event";
        } else {
            eventName = "Some Event";
        }
        return eventName + " [" + e.startTime.toLocaleString() + " - " + e.endTime.toLocaleString() + "]";
    }

    render() {
       var cols = [];
        for (var i = 0; i < this.state.totalTimeUnits; i++) {
            var className = classes.td;
            var title = "";
            var hour = this.state.hours[i];
            if (hour != undefined && hour.events != undefined && hour.events.length > 0) {
                className += " " + hour.class + " + " + classes.tooltip;

                title = this.prepareToolTip(hour);
            } else {
                if (hour != undefined) {
                    title =
                        "No Events Detected" +
                        " [" +
                        hour.startTime.toLocaleString() +
                        " - " +
                        hour.endTime.toLocaleString() +
                        "]";
                    className += " " + classes.Grey + " + " + classes.tooltip;
                }
            }
            cols.push(
                <td key={i} id={i} class={className}>
                    {" "}
                    <div class={classes.tooltip}>
                        <span class={classes.tooltiptext}> {title} </span>{" "}
                    </div>
                </td>
            );
        }

        return (
            <div id={this.props.data.id} key={this.props.data.id} class={classes.Timebar}>
                <table class={classes.TimebarTable}>
                    <tbody>
                        <tr>{cols}</tr>
                    </tbody>
                </table>
            </div>
        );
    }
}

class Last24HoursTimeline extends React.Component {
    constructor(props) {
        super(props);
    }

    componentDidMount() {}

    getDates(startDate, stopDate) {
        var dateArray = new Array();
        var currentDate = startDate;
        while (currentDate <= stopDate) {
            dateArray.push(new Date(currentDate));
            var cdate = new Date(currentDate);
            cdate.setDate(cdate.getDate() + 1);
            currentDate = cdate;
        }
        return dateArray;
    }

    prepareTimebarsData(props) {
        var endTime = new Date();
        var startTime = new Date(endTime);
        startTime.setDate(endTime.getDate() - 1);
       
        var dates = [
            {
                id: 0,
                startTime: startTime,
                endTime: endTime,
                events: [],
                connectionStartTime: props.data.connectionStartTime
            }
        ];

        var events = props.data.events;
        if (!events) {
            events = [
                {
                    startTime: startTime,
                    endTime: endTime
                }
            ];
        }

        events.forEach(e => {
            var matchingDates = findMatchingTimeUnits(e, dates);
            matchingDates.forEach(d => {
                d.events.push(e);
            });
        });
        return dates[0];
    }

    render() {
        var bar = this.prepareTimebarsData(this.props);

        return (
            <div class={classes.Timeline}>
                <Timebar data={bar} timeUnit="MINUTES" />
            </div>
        );
    }
}

class Last7DaysTimeline extends React.Component {
    constructor(props) {
        super(props);
    }

    componentDidMount() {}

    getDates(startDate, stopDate) {
        var dateArray = new Array();
        var currentDate = startDate;
        while (currentDate < stopDate) {
            dateArray.push(new Date(currentDate));
            var cdate = new Date(currentDate);
            cdate.setDate(cdate.getDate() + 1);
            currentDate = cdate;
        }
        return dateArray;
    }

    prepareTimebarsData(props) {
        var endTime = new Date();
        var startTime = new Date(endTime);
        startTime.setDate(endTime.getDate() - 7);
        var events = props.data.events;
        if (!events) {
            events = [
                {
                    startTime: startTime,
                    endTime: endTime
                }
            ];
        }
        var dates = this.getDates(startTime, endTime);
        var bars = [];
        dates = dates.map(function(startDate, idx) {
            var endDate = new Date(startDate);
            endDate.setHours(startDate.getHours() + 24);
            endDate.setMinutes(endDate.getMinutes() - 1);

            return {
                id: idx,
                startTime: startDate,
                endTime: endDate,
                events: [],
                connectionStartTime: props.data.connectionStartTime
            };
        });
        events.forEach(e => {
            var matchingDates = findMatchingTimeUnits(e, dates);
            matchingDates.forEach(d => {
                d.events.push(e);
            });
        });
        return dates;
    }

    render() {
        var bars = this.prepareTimebarsData(this.props);
        var events = this.props.data.events;
        var timebars = bars.map(function(bar, i) {
            return <Timebar data={bar} timeUnit="HOURS" />;
        });

        return <div class={classes.Timeline}>{timebars}</div>;
    }
}

class Last30DaysTimeline extends React.Component {
    constructor(props) {
        super(props);
    }

    componentDidMount() {}

    getDates(startDate, stopDate) {
        var dateArray = new Array();
        var currentDate = startDate;
        while (currentDate < stopDate) {
            dateArray.push(new Date(currentDate));
            var cdate = new Date(currentDate);
            cdate.setDate(cdate.getDate() + 1);
            currentDate = cdate;
        }
        return dateArray;
    }

    prepareTimebarsData(props) {
        var endTime = new Date();
        var startTime = new Date(endTime);
        startTime.setDate(endTime.getDate() - 30);
  
        var dates = this.getDates(startTime, endTime);
        var events = props.data.events;
        if (!events) {
            events = [
                {
                    startTime: startTime,
                    endTime: endTime
                }
            ];
        }
        var bars = [];
        dates = dates.map(function(startDate, idx) {
            var endDate = new Date(startDate);
            endDate.setHours(startDate.getHours() + 24);

            return {
                id: idx,
                startTime: startDate,
                endTime: endDate,
                events: [],
                connectionStartTime: props.data.connectionStartTime
            };
        });
        events.forEach(e => {
            var matchingDates = findMatchingTimeUnits(e, dates);

            matchingDates.forEach(d => {
                d.events.push(e);
            });
        });
        return dates;
    }

    render() {
        var bars = this.prepareTimebarsData(this.props);
        var events = this.props.data.events;
        var timebars = bars.map(function(bar, i) {
            return <Timebar data={bar} timeUnit="HOURS" />;
        });

        return <div class={classes.Timeline}>{timebars}</div>;
    }
}

export { Last7DaysTimeline, Last30DaysTimeline, Last24HoursTimeline };
