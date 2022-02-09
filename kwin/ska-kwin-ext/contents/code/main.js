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

// Yes, hardcoded positions. Good enough for now.
function meetingWindowGeometry() {
    return {
        x: 2560, y: 0,
        width: 1200, height: 777
    };
}

function mainWindowGeometry() {
    return {
        x: 3490, y: 482,
        width: 990, height: 550
    };
}

function zoomApplyCommonLayout() {
    const zooms = getZoomWindows();
    zooms.forEach(function (zwin) {
        if(isMainWindow(zwin)) {
            makeSticky(zwin);
            zwin.geometry = mainWindowGeometry();
        }
        if(isMeetingWindow(zwin)) {
            makeSticky(zwin);
            zwin.geometry = meetingWindowGeometry();
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

// function echoClientInfo() {
//     workspace.clientList().forEach(function (client) {
//         print("- " + client.resourceClass + " " + client.resourceName);
//     });
// }


