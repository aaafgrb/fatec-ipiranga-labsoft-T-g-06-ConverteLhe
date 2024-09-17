let ckey = getCookie("convertelhekey");
if(ckey != ""){
  document.getElementById("apikeyTxt").value = ckey;
}

function handleResponse(r){
  document.getElementById("outTxtBox").value = r.error != "" ? r.error : r.result.join('\n')
}

async function sendRequest() {
  let file = document.getElementById("formFile");
  let response = file.files.length == 0 ? await nonFileRequest() : await fileRequest();

  handleResponse(await response.json());
}

function nonFileRequest(){
  const inData = document.getElementById("inTxtBox");
  var separateLines = inData.value.split(/\r?\n|\r|\n/g);
  return fetch("./api?comp=" + encodeURIComponent(getComp()), {
    method: 'POST',
    body: JSON.stringify({ processThis: separateLines })
  });
}

function fileRequest(){
  const form = document.getElementById("inForm");
  const formData  = new FormData(form);
    
  //todo: insert composition data here
  //formData.append("test", "dataT");
  formData.append("comp", getComp())

  return fetch("./form", {
    method: 'POST',
    body: formData
  });
}

function userButton(){
  location.href="./user"
}

function shareButton(){
  navigator.clipboard.writeText(window.location.host + "/share?comp=" + encodeURIComponent(getComp()));
}

function getComp(){
  //hard coded for now
  return "x1/sapp/$concat/#2";
}

//------------------------------------------------------------------

function getCookie(cname) {
  let name = cname + "=";
  let decodedCookie = decodeURIComponent(document.cookie);
  let ca = decodedCookie.split(';');
  for(let i = 0; i <ca.length; i++) {
    let c = ca[i];
    while (c.charAt(0) == ' ') {
      c = c.substring(1);
    }
    if (c.indexOf(name) == 0) {
      return c.substring(name.length, c.length);
    }
  }
  return "";
}