import React, {
    Component
  } from 'react';
  
import {Line} from 'react-chartjs-2';
import classes from './timeGraph.css'; 
import Moment from 'moment';

  export default class TimeGraph extends Component {
    constructor(props) {
        super(props);
}
componentDidMount() {
}
componentWillUpdate() {

}
componentDidUpdate() {

}

getDataSet() {
  return Object.keys(this.props.dataset).map(key => {
    var result = {}
    result[key] = this.getData(this.props.dataset[key])
    return result;
  })
   
}


getData(values) {
  values = values.filter(v => {
    if(v.value == undefined || v.value == null) {
      return false;
    }
   return Moment(v.timestamp).format('YYYY-MM-DD') == Moment().format('YYYY-MM-DD') 
 })
  var result = values.map(value => {

      return {
          x: Moment(value.timestamp),
          y: value.value
      }
  })
  console.log("result", result);
  return result;
}

getLabels() {
  var values = this.props.dataset['5g'];
  var result = values.map(value => {
      return Moment(value.timestamp, 'HH:mm')
})
result = result.filter(v => {
  return v.format('YYYY-MM-DD') == Moment().format('YYYY-MM-DD')
})
return result;
}

  
    render() {
      var colors = {
        '2g': 'rgb(51, 51, 255)',
        '5g': 'rgb(255, 99, 132)',
        '5gl': 'rgb(255, 159, 64)',
        '5gu': 'rgb(255, 205, 86)',
      }
      var dataSets = this.getDataSet();
      var dataSetValue = dataSets.map(data => {
        var keys = Object.keys(data);
        console.log("color keys:", keys);
        console.log("data:", data[keys[0]]);
        return {
        fill: true,
        lineTension: 0.1,
        borderColor: colors[keys[0]],
        borderDash: [],
        borderDashOffset: 0.0,
        borderJoinStyle: 'miter',
        pointBorderColor: 'rgb(224,224,224)',
        pointBackgroundColor: '#fff',
        pointBorderWidth: 1,
        pointHoverRadius: 5,
        pointHoverBackgroundColor: 'rgb(224,224,224)',
        pointHoverBorderColor: 'rgb(224,224,224)',
        pointHoverBorderWidth: 2,
        pointRadius: 1,
        pointHitRadius: 10,
        data: data[keys[0]]
      }
      })
      var rdata = {
        labels: this.getLabels(),
        datasets: dataSetValue
         
          
      };
      var roption = {
       legend: {
         display: false,
         
       },
         scales: {
            xAxes: [{
                type: 'time',
                time: {
                    unit: 'hour',
                    displayFormats: {
                      hour: 'HH:mm'
                    }
                  
                },
                scaleLabel: {
                    display: true,
                    labelString: 'Date'
                }
            }],
            yAxes: [{
                scaleLabel: {
                    display: true,
                    labelString: 'value'
                }
            }]
         }
       };

      return (

<div className={[classes.BodyRow ,classes.BodyRow1].join(' ')}>
                     
              <div style={{padding:'30px'}}>
                Time Graph
              </div>
              <div className={classes.BodyInfo} ></div>
            <Line data = {rdata} options={roption}/>
            
            </div>

      );
            }}
  