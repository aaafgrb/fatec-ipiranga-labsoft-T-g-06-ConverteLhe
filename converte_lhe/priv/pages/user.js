let cemail = getCookie("convertelheemail");
if(cemail != ""){
  let ckey = getCookie("convertelhekey");
  if(ckey != ""){
    document.getElementById("emailTxt").value = cemail;
    document.getElementById("apikeyTxt").value = ckey;
    getRemLimit();
  }
}

function newUser() {
  makeReq("newUser", 
    (json) => document.getElementById("feedbackLbl").innerHTML = "fetch result: " + json.value
  );
}

function login() {
  let email = document.getElementById("emailTxt").value;
  makeReq("login", 
    (json) => { 
      document.getElementById("apikeyTxt").value = json.value; 
      document.cookie = "convertelhekey=" + json.value;
      document.cookie = "convertelheemail=" + email;
      getRemLimit();
    }
  );
}

function forgotPass() {
  makeReq("newPass", 
    (json) => document.getElementById("feedbackLbl").innerHTML = "fetch result: " + json.value
  );
}

function getRemLimit(){
  makeReq("getLimit", 
    (json) => {document.getElementById("limiteTxt").value = json.value}
  );
}

async function makeReq(req, callback){
  const email = document.getElementById("emailTxt");
  const pass = document.getElementById("passTxt");
  const key = document.getElementById("apikeyTxt")
  
  const feedback = document.getElementById("feedbackLbl")

  const obj = {
      req: req,
      email: email.value,
      key: key.value,
      pass: await digestMessage(await digestMessage(pass.value) + email.value),
  };
  
  const response = fetch("./auseronn", {
    method: 'POST',
    body: JSON.stringify(obj),
  });

  feedback.innerHTML = "fetching...";

  response.then(async (res) => {
    feedback.innerHTML = res.ok ? "fetch success" : "fetch failed";
    callback(await res.json());
  })
}


//---------------------------------------------------------------------

async function digestMessage(message) {
  const msgUint8 = new TextEncoder().encode(message); // encode as (utf-8) Uint8Array
  const hashBuffer = await window.crypto.subtle.digest("SHA-256", msgUint8); // hash the message
  const hashArray = Array.from(new Uint8Array(hashBuffer)); // convert buffer to byte array
  const hashHex = hashArray
    .map((b) => b.toString(16).padStart(2, "0"))
    .join(""); // convert bytes to hex string
  return hashHex;
}

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
