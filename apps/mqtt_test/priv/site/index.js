const API_URL = "http://rpi4.local:8080/weather/720";

// this will be called by browser as soon as
// all necessary resources were loaded and the page
// is ready to render
window.onload = async () => {
    // fetch data from our API
    // we use asynchronous calls to block execution only when necessary
    const data = await (await fetch(API_URL)).json();
    //const data1 = data2.sort((a,b) => a.timestamp.localeCompare(b.timestamp));
    //const data = data2.slice(-720);

    // convert timestamps to the label format we want to use in our charts
    const labels = data.map(item => moment(item.timestamp).format('MM/DD HH:mm'));//.sort().reverse();

    // render chart
    let chart = createChart(labels, data);

    // create handlers for radio buttons that can be used to select different metrics
    // that will be displayed on the chart
    document.getElementById("temp_radio").onclick = () => {
        // this call will prepare data to be used in a 
        // format that chart.js expects as an input
        const tempData = createChartDataset(data, 'temp');

        // this function will update the initialized chart with
        // new data, along with its minimal and maximal bounds
        updateChart(chart, tempData, tempData.min - 5, tempData.max + 5);
    };

    document.getElementById("pressure_radio").onclick = () => {
        const pressureData = createChartDataset(data, 'pressure');
        updateChart(chart, pressureData, 700, pressureData.max + 10);
    };

    document.getElementById("humidity_radio").onclick = () => {
        const humidityData = createChartDataset(data, 'humidity');
        updateChart(chart, humidityData, 0, 100);
    };

    // set default metric to temperature
    document.getElementById('temp_radio').click();
};

/**
 * Creates a new chart
 * @param {Array.<string>} labels
 * @param {Array} data
 */
function createChart(labels, data) {
    let ctx = document.getElementById('tempChart').getContext('2d');
    // Set colors and fonts
    Chart.defaults.global.defaultFontColor = '#636160ff';
    Chart.defaults.global.defaultFontFamily = '"Open Sans", sans-serif';
    Chart.defaults.global.defaultFontSize = 20;

    // Create chart
    let chart = new Chart(ctx, {
        "type": "line",
        "data": {
            "labels": labels,
            "datasets": [{
                "label": "",
                "data": data,
                "fill": false,
                "borderColor": "#6e2594ff",
                "lineTension": 0.1
            }]
        },
        "options": {
            "legend": {
                "display": false
            },
            "aspectRatio": 1,
            "maintainAspectRatio": false,
            "scales": {
                "yAxes": [{
                    "offset": true,
                    "gridLines": {
                        "display": false
                    },
                    "ticks": {
                        "suggestedMin": 0,
                        "suggestedMax": 35
                    }
                }],
                "xAxes": [{
                    "offset": true,
                    "gridLines": {
                        "display": false
                    }
                }]
            }
        }
    });

    return chart;
}

/**
 * Transforms API response into an object that can be used in the {@link updateChart} call.
 * @param {Object} data
 * @param {string} selector
 */
function createChartDataset(data, selector) {
    let series = data.map(item => item[selector]);
    return {
        'dates': data.map(item => item.timestamp),
        'data': series,
        'min': Math.min.apply(series),
        'max': Math.max.apply(series)
    };
}

/**
 * Updates given chart using data and sets y axis min and max values
 * @param {ChartJS object} chart
 * @param {Object} data
 * @param {number} min
 * @param {number} max
 */
function updateChart(chart, data, min, max) {
    chart.data.datasets[0].data = data.data;
    chart.data.datasets[0].labels = data.dates.map(item => moment(item.timestamp).format('ddd HH a'));
    chart.options.scales.yAxes[0].ticks.suggestedMin = min;
    chart.options.scales.yAxes[0].ticks.suggestedMax = max;
    chart.update();

}


