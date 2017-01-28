import QtQuick 2.3
import QtQuick.Controls 1.4
import QtQuick.Dialogs 1.0
import QtQuick.Layouts 1.3

ApplicationWindow {
  visible: true
  onClosing: quit()
  id: root
  width: 600
  height: 700

  Timer {
    interval: 100
    running: true
    repeat: true
    onTriggered: {
      if(auto.checked)
        nextTrajectory();
    }
  }

  Component.onCompleted: {
    initialize()
  }

  FileDialog {
    id: maskDialog
    folder: currentDirectory
    title: "Loas Mask"
    onAccepted: loadWall(fileUrls[0])
  }

  FileDialog {
    id: saveDialog
    folder: currentDirectory
    title: "Save Canvas"
    onAccepted: {
      if(!roomCanvas.save(fileUrls[0].slice('file://'.length))) {
        console.log("could not save the image");
      }
    }
    selectExisting: false
  }

  Row {
    id: controls
    spacing: 5

    GridLayout {
      columns: 2
      Layout.alignment: Qt.AlignVCenter

      Text{text: "min"}
      TextField {
        text: defMinLength
        textColor: minLength ? "red" : "black"
        onEditingFinished: setMinLength(text)
      }

      Text{text: "max"}
      TextField {
        text: defMaxLength
        textColor: maxLength ? "red" : "black"
        onEditingFinished: setMaxLength(text)
      }

      Text {
        textFormat: Text.RichText
        text: "v<sub>0</sub>"
      }
      TextField {
        text: defInitVelocity
        textColor: initVelocity ? "red" : "black"
        onEditingFinished: setInitVelocity(text)
      }

      Text{text: "Δv"}
      TextField {
        id: accel
        text: defAccelerationFormula
        textColor: accelerationFormula ? "red" : "black"
        onEditingFinished: setAccelerationFormula(text, a_accel.text)
      }

      Text{text: "Δα"}
      TextField {
        id: a_accel
        text: defAngularAccelerationFormula
        textColor: accelerationFormula ? "red" : "black"
        onEditingFinished: setAngularAccelerationFormula(accel.text, text)
      }
    }

    Rectangle {
      color: "darkgrey"
      anchors.top: parent.top
      anchors.bottom: parent.bottom
      width: 3
    }

    Rectangle {
      color: "darkgrey"
      anchors.top: parent.top
      anchors.bottom: parent.bottom
      width: 3
    }

    Rectangle {
      color: "darkgrey"
      anchors.top: parent.top
      anchors.bottom: parent.bottom
      width: 3
    }

    Column {
      Row {
        Button {
          text: "Load Mask"
          onClicked: maskDialog.open()
        }

        Button {
          text: "Recompile Config"
          onClicked: recompileConfig()
        }
      }

      Row {
        Button {
          text: "Clear"
          onClicked: {
            roomCanvas.clear = true;
            roomCanvas.requestPaint();
          }
        }

        Button {
          text: "Generate"
          onClicked: nextTrajectory()
          activeFocusOnPress: true
        }

        CheckBox {
          id: auto
          text: "Auto"
          activeFocusOnPress: true
        }
      }

      Row {
        Button {
          text: "Save"
          onClicked: saveDialog.open()
        }
      }
    }
  }

  Rectangle {
    anchors.top:    controls.bottom
    anchors.left:   parent.left
    anchors.right:  parent.right
    anchors.bottom: parent.bottom

    Image {
      anchors.fill: parent
      source:       roomPath
    }

    Canvas {
      id: roomCanvas
      anchors.fill: parent

      property var clear: false

      property var traj: trajectory
      onTrajChanged: requestPaint()

      onPaint: {
        var ctx = getContext("2d");

        if(this.clear) {
          this.clear = false;
          ctx.fillStyle = 'rgba(0, 0, 0, 0)';
          ctx.clearRect(0, 0, width, height);
        }
        else {
          var pts = traj;
          if(pts.length > 0) {
            var x = pts[0][0] * width;
            var y = pts[0][1] * height;
            pts.forEach(function(pt) {
              ctx.beginPath();

              ctx.moveTo(x, y);
              x = pt[0] * width;
              y = pt[1] * height;
              ctx.lineTo(x, y);

              for(var i = 2; i < 5; i++) { pt[i] = Math.round(pt[i]*255) }

              ctx.strokeStyle = 'rgba(' + pt.slice(2,6).join(', ') + ')';
              ctx.lineWidth = pt[6];
              // ctx.lineCap = "round";
              ctx.stroke();
            });
          }
        }
      }
    }
  }
}
