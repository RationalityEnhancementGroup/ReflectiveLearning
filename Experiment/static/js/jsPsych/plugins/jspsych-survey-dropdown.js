/**
 * jspsych-survey-text
 * a jspsych plugin for free response survey questions
 *
 * Josh de Leeuw
 *
 * documentation: docs.jspsych.org
 *
 */


jsPsych.plugins['survey-dropdown'] = (function() {

  var plugin = {};

  plugin.trial = function(display_element, trial) {

    trial.preamble = typeof trial.preamble == 'undefined' ? "" : trial.preamble;
    trial.validate = typeof trial.validate == 'undefined' ? false : trial.validate;
    if (typeof trial.rows == 'undefined') {
      trial.rows = [];
      for (var i = 0; i < trial.questions.length; i++) {
        trial.rows.push(3);
      }
    }
    if (typeof trial.columns == 'undefined') {
      trial.columns = [];
      for (var i = 0; i < trial.questions.length; i++) {
        trial.columns.push(60);
      }
    }

    // if any trial variables are functions
    // this evaluates the function and replaces
    // it with the output of the function
    trial = jsPsych.pluginAPI.evaluateFunctionParameters(trial);

    // show preamble text
    display_element.append($('<div>', {
      "id": 'jspsych-survey-text-preamble',
      "class": 'jspsych-survey-text-preamble'
    }));

    $('#jspsych-survey-text-preamble').html(trial.preamble);

    // add questions
    for (var i = 0; i < trial.questions.length; i++) {
      // create div
      display_element.append($('<div>', {
        "id": 'jspsych-survey-text-' + i,
        "class": 'jspsych-survey-text-question'
      }));

      // add question text
      $("#jspsych-survey-text-" + i).append('<p class="jspsych-survey-text">' + trial.questions[i] + '</p>');

      // add text box
      //$("#jspsych-survey-text-" + i).append('<textarea name="#jspsych-survey-text-response-' + //i + '" cols="' + trial.columns[i] + '" rows="' + trial.rows[i] + '"></textarea>');

      // add dropdown menu
      let content = '<select style="background-color: lightGrey" class="btn"  name="#jspsych-survey-text-response-' + i + '" cols="' + trial.columns[i] + '" rows="' + trial.rows[i] + '">';

      // add defualt option
      content = content + '<option>Please select an option</option>'

      options = trial.options[i]
      for(var j in options){
        content = content + '<option>'+ options[j] +' </option>'
      }
      content = content + "</select>";

      $("#jspsych-survey-text-" + i).append(content);
    }

    // add submit button
    display_element.append($('<button>', {
      'id': 'jspsych-survey-text-next',
      'class': 'btn btn-primary btn-lg'
    }));
    $("#jspsych-survey-text-next").html(trial.button);
    $("#jspsych-survey-text-next").click(function() {

      // create object to hold responses
      var question_data = {};
      var vals = []
      $("div.jspsych-survey-text-question").each(function(index) {
        var id = "Q" + index;
        var val = $(this).children('select').val();
        var obje = {};
        obje[id] = val;
        $.extend(question_data, obje);
        vals.push(val);
      });

      // check if selection was made: validation
      if(trial.validate && vals.includes('Please select an option')){
        alert('Please select an option for each question.')
        return;
      }

      // measure response time
      var endTime = (new Date()).getTime();
      var response_time = endTime - startTime;

      // save data
      var trialdata = {
        "rt": response_time,
        "responses": JSON.stringify(question_data)
      };

      display_element.html('');

      // next trial
      jsPsych.finishTrial(trialdata);
    });

    var startTime = (new Date()).getTime();
  };

  return plugin;
})();
