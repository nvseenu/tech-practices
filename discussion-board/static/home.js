 $(document).ready(function(){
$('#login').click(function(event) {

            var modal = $("#loginModal")
            var name = modal.find('#name').val()
            var email = modal.find('#email').val()

            var jqxhr = $.post("/discussion-board/login", {
                    name: name,
                    email: email
                })
                .done(function() {
                    modal.modal("hide")
                    window.location.href = "/discussion-board/postQuestionForm"
                    })
        })
      })
