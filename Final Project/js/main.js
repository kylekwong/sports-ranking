var scores  = [];
var newgame = [];

var t1Input  = document.getElementById("team1");
var s1Input   = document.getElementById("score1");
var t2Input = document.getElementById("team2");
var s2Input = document.getElementById("score2")

var messageBox  = document.getElementById("display");

function insert () {

    var sisiList = ["(" + t1Input.value + "," + s1Input.value + ")" + ";" + "(" + t2Input.value + "," + s2Input.value + ")"]
    var newgameList = [t1Input.value + " : " + s1Input.value + " --  " + t2Input.value + " : " + s2Input.value]

    scores.push(sisiList);
    newgame.push(newgameList);

    console.log(scores)

    clearAndShow();
}

function clearAndShow () {
  // Clear our fields
  t1Input.value = "";
  s1Input.value = "";
  t2Input.value = "";
  s2Input.value = "";
  
  // Show our output
  //messageBox.innerHTML = "";
  
  messageBox.innerHTML += "" + newgame + "<br/>";

  newgame = [];
}

function save () {

    saveToFile(scores,"sportData.json")

}


var saveToFile = function(object, filename){
    var blob, blobText;
    blobText = [JSON.stringify(object)];
    blob = new Blob(blobText, {
        type: "text/plain;charset=utf-8"
    });
    saveAs(blob, filename);
}