// magic.js


// process the form
$.ajax({
    type        : 'POST', // define the type of HTTP verb we want to use (POST for our form)
    url         : 'process.php', // the url where we want to POST
    data        : formData, // our data object
    async       : true,
    dataType    : 'json' // what type of data do we expect back from the server
})
    // using the done promise callback
    .done(function(data) {

        // log data to the console so we can see
        console.log(data);

        // here we will handle errors and validation messages
        if ( ! data.success) {

            // handle errors for name ---------------
            if (data.errors.login) {
                $('#login-group').addClass('has-error'); // add the error class to show red input
                $('#login-group').append('<div class="help-block">' + data.errors.login + '</div>'); // add the actual error message under our input
            }

            // handle errors for email ---------------
            if (data.errors.password) {
                $('#password-group').addClass('has-error'); // add the error class to show red input
                $('#password-group').append('<div class="help-block">' + data.errors.password + '</div>'); // add the actual error message under our input
            }

            // handle errors for superhero alias ---------------
            if (data.errors.repeat) {
                $('#repeat-group').addClass('has-error'); // add the error class to show red input
                $('#repeat-group').append('<div class="help-block">' + data.errors.repeat + '</div>'); // add the actual error message under our input
            }

            if (data.errors.birth) {
                $('#birth-group').addClass('has-error'); // add the error class to show red input
                $('#birth-group').append('<div class="help-block">' + data.errors.birth + '</div>'); // add the actual error message under our input
            }

        } else {

            // ALL GOOD! just show the success message!
            $('form').append('<div class="alert alert-success">' + data.message + '</div>');

            // usually after form submission, you'll want to redirect
            // window.location = '/thank-you'; // redirect a user to another page
            alert('success'); // for now we'll just alert the user

        }

    });

