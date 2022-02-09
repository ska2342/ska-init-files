var previous_geometries = [];

function makeSticky(client) {
    client.onAllDesktops = true;
}

function clientCaptionFind(client, needle) {
    if(client.caption.indexOf(needle) !== -1) {
        return true;
    } else {
        return false;
    }
}

function isZoomWindow(client) {
    if(client.resourceClass == "zoom") {
        return true;
    } else {
        return false;
    }
}

function getZoomWindows() {
    const zoom_wins = [];
    workspace.clientList().forEach(function (cli) {
        if(clientCaptionFind(cli,'Zoom')) {
            zoom_wins.push(cli);
        }
    });
    return zoom_wins;
}

function isMeetingWindow(client) {
    if(clientCaptionFind(client, 'Meeting')
       &&
       !clientCaptionFind(client, 'Chromium')) {
        return true;
    } else {
        return false;
    }
}
function isMainWindow(client) {

    if(clientCaptionFind(client, '<')) {
        return true;
    } else {
        return false;
    }
}

function toRect(geom) {
    return {
        x: geom.x,
        y: geom.y,
        width: geom.width,
        height: geom.height
    };
}

function geomToString(g) {
    if (g === null || g === undefined) {
        return "no geometry";
    } else {
        var s =  "Geometry(";
        s = s + "x:" + g.x + ", ";
        s = s + "y:" + g.y + ", ";
        s = s + "w:" + g.width + ", ";
        s = s + "h:" + g.height + ", ";
        s = s + ")";
        return s;
    }
    // return "geomTostring" + g;
}

// Yes, hardcoded positions. Good enough for now.
function meetingWindowGeometry(client) {
    if (previous_geometries[0] === null
        || previous_geometries[0] === undefined) {
        previous_geometries[0] = toRect(client.geometry);
        return {
            x: 2560, y: 0,
            width: 1200, height: 777
        };
    } else {
        var geo = previous_geometries[0];
        previous_geometries[0] = null;
        return geo;
    }
}

function mainWindowGeometry(client) {
    if (previous_geometries[1] === null
        || previous_geometries[1] === undefined) {
        previous_geometries[1] = toRect(client.geometry);
        return {
            x: 3490, y: 482,
            width: 990, height: 550
        };
    } else {
        var geo = previous_geometries[1];
        previous_geometries[1] = null;
        return geo;
    }
}

function zoomApplyCommonLayout() {
    print("Geo0:" + geomToString(previous_geometries[0]));
    print("Geo1:" + geomToString(previous_geometries[1]));
    // print("Screens:" + workspace.numScreens);
    const zooms = getZoomWindows();
    zooms.forEach(function (zwin) {
        if(isMainWindow(zwin)) {
            makeSticky(zwin);
            zwin.geometry = mainWindowGeometry(zwin);
        }
        if(isMeetingWindow(zwin)) {
            makeSticky(zwin);
            zwin.geometry = meetingWindowGeometry(zwin);
        }
    });
}

function onClientAddedArrangeZoom(client) {
    if(workspace.numScreens > 1
       && isZoomWindow(client)
       && !clientCaptionFind(client, 'Chat')) {
        zoomApplyCommonLayout();
    }
}
registerShortcut (
    "ZoomWindowsLayoutMain",
    "Arrange Zoom windows on right screen",
    "Ctrl+Meta+Z",
    function () { zoomApplyCommonLayout(); }
);
workspace.clientAdded.connect(onClientAddedArrangeZoom);
previous_geometries = [];

// function echoClientInfo() {
//     workspace.clientList().forEach(function (client) {
//         print("- " + client.resourceClass + " " + client.resourceName);
//     });
// }

