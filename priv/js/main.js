/*
 Written by Bill Wang <freecnpro@gmail.com>
*/

function loadCharts(){
    
    var schedulerobj, ioobj;
    var schedulerChart, memoryChart, ioChart;

    Highcharts.setOptions({
        global: {
            useUTC: false
        },
        colors: ['#CC0000', '#00FF00', '#0000FF', '#FF9655', '#24CBE5', '#AA66CC', '#99CC00', '#669900']
    });
    
    schedulerChart = new Highcharts.Chart({
        chart: {
            renderTo: 'scheduler-utilization',
            type: 'spline',
            animation: Highcharts.svg,
            marginRight: 10,
            events: {
                load: function() {
                    var xmlhttp = new XMLHttpRequest();
                    setInterval(function() {
                        sendAsyncRequest(xmlhttp, "action=get_perf&type=scheduler", function() {
                            if (xmlhttp.readyState == 4 && xmlhttp.status == 200) {
                                var newData = eval("(" + xmlhttp.responseText + ")");
                                for (var i = 0; i < schedulerobj.scheduler.length; i++) {
                                    var series = schedulerChart.series[i];
                                    var x = (new Date()).getTime(),
                                        activetime = newData.scheduler[i].activetime - schedulerobj.scheduler[i].activetime,
                                        totaltime = newData.scheduler[i].totaltime - schedulerobj.scheduler[i].totaltime,
                                        y = Math.floor((100 * activetime) / totaltime);
                                    series.addPoint([x, y], true, true);
                                }
                                schedulerobj = newData;
                            }
                        });
                    }, 1000);
                }
            }
        },
        title: {
            text: 'Scheduler Utilization(%)',
            align: 'left'
        },
        xAxis: {
            type: 'datetime',
            tickPixelInterval: 100
        },
        yAxis: {
            floor: 0,
            ceiling: 100,
            title: null
        },
        tooltip: {
            enabled: false
        },
        plotOptions: {
            spline: {
                marker: {
                    enabled: false
                }
            }
        },
        series: (function() {
            var seriesdata = [];
            var responseText = sendSyncRequest("action=get_perf&type=scheduler");
            schedulerobj = eval("(" + responseText + ")");
            for (var i = 0; i < schedulerobj.scheduler.length; i++) {
                seriesdata.push({
                    name: i + 1,
                    data: (function() {
                        var data = [],
                            time = (new Date()).getTime(),
                            j;

                        for (j = -10; j <= 0; j += 1) {
                            data.push({
                                x: time + j * 1000,
                                y: 0
                            });
                        }
                        return data;
                    }())
                });
            }
            return seriesdata;
        }())
    });
    memoryChart = new Highcharts.Chart({
        chart: {
            renderTo: 'memory-usage',
            type: 'spline',
            animation: Highcharts.svg,
            marginRight: 10,
            events: {
                load: function() {
                    var xmlhttp = new XMLHttpRequest();
                    setInterval(function() {
                        sendAsyncRequest(xmlhttp, "action=get_perf&type=memory", function() {
                            if (xmlhttp.readyState == 4 && xmlhttp.status == 200) {
                                var data = eval("(" + xmlhttp.responseText + ")");
                                var x = (new Date()).getTime();
                                var values = new Array(data.total,data.processes,data.atom,data.binary,data.code,data.ets);
                                var max = (values.sort(function(a, b){
                                    return b - a;
                                }))[0];
                                console.log("memory max: %d", max);
                                memoryChart.setTitle({text: getTitle("Memory Usage", max)});
                                for(var i = 0; i < values.length; i++){
                                    memoryChart.series[i].addPoint([x, getBetterValue(values[i], max)], true, true);
                                }
                            }
                        });
                    }, 1000);
                }
            }

        },
        title: {
            text: 'Memory Usage(KB)',
            align: 'left'
        },
        xAxis: {
            type: 'datetime',
            tickPixelInterval: 100
        },
        yAxis: {
            floor: 0,
            title: null
        },
        tooltip: {
            enabled: false
        },
        plotOptions: {
            spline: {
                marker: {
                    enabled: false
                }
            }
        },
        series: (function() {
            var seriesdata = [];
            var names = new Array("Total", "Processes", "Atom", "Binary", "Code", "Ets");
            for (var i = 0; i < 6; i++) {
                seriesdata.push({
                    name: names[i],
                    data: (function() {
                        var data = [],
                            time = (new Date()).getTime(),
                            j;

                        for (j = -10; j <= 0; j += 1) {
                            data.push({
                                x: time + j * 1000,
                                y: 0
                            });
                        }
                        return data;
                    }())
                });
            }
            return seriesdata;
        }())
    });
    ioChart = new Highcharts.Chart({
        chart: {
            renderTo: 'io-usage',
            type: 'spline',
            animation: Highcharts.svg,
            marginRight: 10,
            events: {
                load: function() {
                    var xmlhttp = new XMLHttpRequest();
                    setInterval(function() {
                        sendAsyncRequest(xmlhttp, "action=get_perf&type=io", function() {
                            if (xmlhttp.readyState == 4 && xmlhttp.status == 200) {
                                var data = eval("(" + xmlhttp.responseText + ")");
                                var x = (new Date()).getTime();
                                var input = data.input - ioobj.input;
                                var output = data.output - ioobj.output;
                                var max = (input > output ? input : output);
                                ioChart.setTitle({text: getTitle("IO Usage", max)});
                                ioChart.series[0].addPoint([x, getBetterValue(input, max)], true, true);
                                ioChart.series[1].addPoint([x, getBetterValue(output, max)], true, true);
                                ioobj = data;
                            }
                        });
                    }, 1000);
                }
            }

        },
        title: {
            text: 'IO Usage(B)',
            align: 'left'
        },
        xAxis: {
            type: 'datetime',
            tickPixelInterval: 100
        },
        yAxis: {
            floor: 0,
            title: null
        },
        tooltip: {
            enabled: false
        },
        plotOptions: {
            spline: {
                marker: {
                    enabled: false
                }
            }
        },
        series: (function() {
            var seriesdata = [];
            var responseText = sendSyncRequest("action=get_perf&type=io");
            ioobj = eval("(" + responseText + ")");
            var names = new Array("Input", "Output");
            for (var i = 0; i < 2; i++) {
                seriesdata.push({
                    name: names[i],
                    data: (function() {
                        var data = [],
                            time = (new Date()).getTime(),
                            j;

                        for (j = -10; j <= 0; j += 1) {
                            data.push({
                                x: time + j * 1000,
                                y: 0
                            });
                        }
                        return data;
                    }())
                });
            }
            return seriesdata;
        }())
    });
}

function loadSysInfo(){
    loadSysInfos();
    setInterval(function(){
        loadSysInfos();
    }, 10*1000);
}

function loadSysInfos() {
    var xmlhttp = new XMLHttpRequest();
    sendAsyncRequest(xmlhttp, "action=get_sys", function() {
        if (xmlhttp.readyState == 4 && xmlhttp.status == 200) {
            var jsonData = eval("(" + xmlhttp.responseText + ")");
            var datas = new Array(jsonData.system, jsonData.memory, jsonData.cpu, jsonData.statistics, jsonData.alloctor);
            var ids = new Array("#system-architecture", "#memory-info", "#cpu-threads", "#statistics", "#alloctor-table");
            for (var i = 0; i < datas.length; i++) {
                displayInfo(ids[i], datas[i]);
            }
        }
    });
}

function displayInfo(id, data) {
    var txt = "";
    if (id == "#alloctor-table") {
        for (var i = 0; i < data.length; i++) {
            txt = txt + "<tr><td>" + data[i].name + "</td><td>" + data[i].bs + "</td><td>" + data[i].cs + "</td></tr>";
        }
    } else {
        for (var i = 0; i < data.length; i++) {
            txt = txt + "<tr><td>" + data[i].name + "</td><td>" + data[i].value + "</td></tr>";
        }
    }
    $(id).html(txt);
}

function sendAsyncRequest(xmlhttp, qs, fun) {
    xmlhttp.onreadystatechange = fun;
    xmlhttp.open("POST", "info", true);
    xmlhttp.send(qs);
}

function sendSyncRequest(qs) {
    var xmlhttp = new XMLHttpRequest();
    xmlhttp.open("POST", "info", false);
    xmlhttp.send(qs);
    return xmlhttp.responseText;
}

function getTitle(title, max){
    kb = Math.floor(max / 1024);
    mb = Math.floor(kb / 1024);
    gb = Math.floor(mb / 1024);
    if(gb > 10){
        return title+"(GB)";
    }else if(mb > 10){
        return title+"(MB)";
    }else if(kb > 0){
        return title+"(KB)";
    }else{
        return title+"(B)";
    }
}

function getBetterValue(value, max){
    kb = Math.floor(max / 1024);
    mb = Math.floor(kb / 1024);
    gb = Math.floor(mb / 1024);
    if(gb > 10){
        return Math.floor(value / (1024*1024*1024));
    }else if(mb > 10){
        return Math.floor(value / (1024*1024));
    }else if(kb > 0){
        return Math.floor(value / 1024);
    }else{
        return value;
    }
}

function connect_node(){
    $('#connect_node_modal').modal('toggle');
    var nodename = $('#nodename').val();
    var cookie = $('#cookie').val();
    var qs = "action=connect_node&node=" + nodename + "&cookie=" + cookie;
    document.getElementById("connect_node_form").reset();
    var xmlhttp = new XMLHttpRequest();
    sendAsyncRequest(xmlhttp, qs, function(){
        if (xmlhttp.readyState == 4 && xmlhttp.status == 200) {
            if(xmlhttp.responseText == "Connect failed"){
                alert("Connect failed!");
            }else{
                location.reload();
            }
        }
    });
}

function get_nodes(){
    var xmlhttp = new XMLHttpRequest();
    sendAsyncRequest(xmlhttp, "action=get_nodes", function(){
        if (xmlhttp.readyState == 4 && xmlhttp.status == 200) {
            var jsonData = eval("(" + xmlhttp.responseText + ")");
            var nodes = jsonData.nodes;
            var txt = "";
            for(var i = 0; i < nodes.length; i++){
                    txt = txt + "<li role=\"presentation\" id=\"node_item\"><a role=\"menuitem\" href=\"#\">" + nodes[i] + "</a></li>";
            }
            txt = txt + "<li role=\"presentation\"><a role=\"menuitem\" href=\"#connect_node_modal\" data-toggle=\"modal\">Connect Node</a></li>";
            $('#nodes').html(txt);
        }
    });
}

function change_node(node){
    var xmlhttp = new XMLHttpRequest();
    sendAsyncRequest(xmlhttp, "action=change_node&node=" + node, function(){
        if (xmlhttp.readyState == 4 && xmlhttp.status == 200) {
            console.log("Respone: ", xmlhttp.responseText);
            if(xmlhttp.responseText == "true"){
                location.reload();
            }
        }
    });
}