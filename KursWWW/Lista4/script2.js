
mySubmit = document.getElementById("submit");
var accInput = document.getElementById("konto");
var disableArr = [true, true, true, true];
checkDisabled();
accInput.addEventListener("blur", function()
{

    var accStr = accInput.value;

    var length = accStr.replace(/\s/g, "").length;
    if (length !== 16)
    {
        accInput.style.backgroundColor = "red";
        alert("Podaj prawidłowy numer konta");
    }
    else 
    {
        accInput.style.backgroundColor = "green";
        disableArr[0] = false;
        checkDisabled();
    }
});

var emailInput = document.getElementById("email");
emailInput.addEventListener("blur", function()
{
    var validator = /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;

    if (!validator.test(emailInput.value) || emailInput.value=="" || emailInput.value==null)
    {
        emailInput.style.backgroundColor = "red";
        alert("Podaj prawidłowy adres e-mail");
    }
    else{
        emailInput.style.backgroundColor = "green";
        disableArr[1] = false;
        checkDisabled();
    }
});

var birthInput = document.getElementById("data");
birthInput.addEventListener("blur", function()
{
    var validator = /^(?:(?:31(\/|-|\.)(?:0?[13578]|1[02]))\1|(?:(?:29|30)(\/|-|\.)(?:0?[1,3-9]|1[0-2])\2))(?:(?:1[6-9]|[2-9]\d)?\d{2})$|^(?:29(\/|-|\.)0?2\3(?:(?:(?:1[6-9]|[2-9]\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))$|^(?:0?[1-9]|1\d|2[0-8])(\/|-|\.)(?:(?:0?[1-9])|(?:1[0-2]))\4(?:(?:1[6-9]|[2-9]\d)?\d{2})$/;

    if (!validator.test(birthInput.value) || birthInput.value=="" || birthInput.value==null)
    {
        birthInput.style.backgroundColor = "red";
        alert("Podaj prawidłową datę urodzenia w odpowiednim formacie");
    }
    else{
        birthInput.style.backgroundColor = "green";
        disableArr[2] = false;
        checkDisabled();
    }
});

var peselInput = document.getElementById("pesel");
peselInput.addEventListener("blur", function()
{
    var peselStr = peselInput.value;
    if (peselStr.length !== 11)
    {
        peselInput.style.backgroundColor = "red";
        alert("Podaj prawidłowy numer PESEL");
    }
    else{
        peselInput.style.backgroundColor = "green";
        disableArr[3] = false;
        checkDisabled();
    }
});


function checkDisabled()
{
    if (disableArr.every(function(element){return element === false;})) 
    {
        mySubmit.disabled = false;
    }
    else 
    {
        mySubmit.disabled = true;
    }
}