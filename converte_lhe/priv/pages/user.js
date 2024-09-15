function newUser() {
  makeReq("newUser", 
    (json) => document.getElementById("feedbackLbl").innerHTML = "fetch result: " + json.value
  );
}

function login() {
  makeReq("login", 
    (json) => { 
      document.getElementById("apikeyTxt").value = json.value; 
      document.getElementById("limiteTxt").value = "unimplemented"; 
    }
  );
}

function forgotPass() {
  makeReq("newPass", 
    (json) => document.getElementById("feedbackLbl").innerHTML = "fetch result: " + json.value
  );
}

async function makeReq(req, callback){
  const email = document.getElementById("emailTxt");
  const pass = document.getElementById("passTxt");
  
  const feedback = document.getElementById("feedbackLbl")

  const obj = {
      req: req,
      email: email.value,
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

async function digestMessage(message) {
  const msgUint8 = new TextEncoder().encode(message); // encode as (utf-8) Uint8Array
  const hashBuffer = await window.crypto.subtle.digest("SHA-256", msgUint8); // hash the message
  const hashArray = Array.from(new Uint8Array(hashBuffer)); // convert buffer to byte array
  const hashHex = hashArray
    .map((b) => b.toString(16).padStart(2, "0"))
    .join(""); // convert bytes to hex string
  return hashHex;
}
