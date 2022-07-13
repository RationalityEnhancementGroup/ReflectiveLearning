// Generated by CoffeeScript 2.4.1
// coffeelint: disable=max_line_length, indentation
var BLOCKS, COLORS, CONDITION, DEBUG, DEMO, DEMO_TRIALS, NUM_CONTROL_BLOCKS, NUM_EACH, NUM_MOUSELAB_TRIALS, N_TRIAL,
    OBJECT_LEVEL_PR, PARAMS, SCORE, SHOW_PARTICIPANT, STRUCTURE_TEST, STRUCTURE_TRAINING, STRUCTURE_TRANSFER, TALK,
    TRAINING_SCORE, TRIALS_TEST, TRIALS_TRAINING, TRIALS_TRANSFER, prompt_length, REPETITIONS, RECENT_SCORE1, RECENT_SCORE2, RECENT_SCORE3, calculateBonus, createStartButton, getTestTrials,
    getTrainingTrials, getTransferTrials, initializeExperiment, psiturk, saveData, with_feedback, with_prompt,
    with_transfer_block;

var EXCLUDE = false;
TALK = false;
SHOW_PARTICIPANT = false;
RECENT_SCORE1 = 0; // last 3 rounds
RECENT_SCORE2 = 0; // 3 rounds before that
RECENT_SCORE3 = 0; // all rounds before that
REPETITIONS = 0;


// ---------------- configuration
console.log(condition);
CONDITION = parseInt(condition);

// 4 CONDITIONS
if(CONDITION == 3){
  CONDITION = 2
}

DEBUG = false;

//CONDITION = parseInt(window.prompt('condition0: Passive, condition1: Active, condition2: Reflection', 0));
//DEBUG = window.prompt('Run through input and quiz block + reduced number of trials ? -> 1', 0) == 1;
// 0: passive control, 1: active control, 2: experimental

reveal_rewards = true;
with_feedback = false;
prompt_length  = 3;

console.log(`CONDITION: ${CONDITION}.`);
console.log(`with_feedback: ${with_feedback}.`);


if (DEBUG) {
    prompt_length = 2;
    console.log("X X X X X X X X X X X X X X X X X\n X X X X X DEBUG  MODE X X X X X\nX X X X X X X X X X X X X X X X X");

} else {
    console.log("# =============================== #\n# ========= NORMAL MODE ========= #\n# =============================== #");
}
if (mode === "{{ mode }}") {
    DEMO = true;
}
PAY_BASE = '£2.00'
PAY_MEAN = '£3.00'
PAY_MAX = '£2.00'

PARAMS = void 0;

TRIALS_TRAINING = void 0;

TRIALS_TEST = void 0;

TRIALS_TRANSFER = void 0;

DEMO_TRIALS = void 0;

STRUCTURE_TRAINING = void 0;

STRUCTURE_TEST = void 0;

STRUCTURE_TRANSFER = void 0;

SCORE = 0;

TRAINING_SCORE = 0;


var getQuizInfo = function () {
  var div = document.getElementById("quizInfoDiv");
  var button = document.getElementById("quizInfoButton");
  if (div.innerHTML === "") {
    infos = ["In this experiment, you will be playing a game called Web of Cash.",
    "During the game, you will navigate a spider across a web of nodes.",
    "Each nodes has an underlying value that tells you how much reward the spider collects by visiting that node.",
    "Your goal in this game is to reach one of the final nodes while maximizing the rewards collected.",
    "It is hard to decide which node to visit when you don't know the rewards.",
    "Fortunately, you can click on a node to reveal the underlying reward.",
    "Planning isn't free: Each click costs $1.",
    "To move the spider, use the arrow keys.",
    "You will play multiple rounds of the game. In each round, the rewards on the web will be different.",
    "The better you perform in the game, the higher your bonus will be."
    ]

    let content = '<ul>';
    for(q in infos){
      content = content + "<li>" + infos[q] + "</li>";
    }
    content = content + "</ul>"

    button.innerHTML = 'Hide instructions';
    div.innerHTML = content;
  } else {
    button.innerHTML = 'Show instructions'
    div.innerHTML = "";
  }
}

calculateBonus = void 0;

getTrainingTrials = void 0;

getTestTrials = void 0;

getTransferTrials = void 0;

psiturk = new PsiTurk(uniqueId, adServerLoc, mode);

psiturk.recordUnstructuredData('condition', CONDITION);

psiturk.recordUnstructuredData('with_feedback', with_feedback);

psiturk.recordUnstructuredData('with_prompt', with_prompt);


saveData = function () {
    return new Promise(function (resolve, reject) {
        var timeout;
        timeout = delay(10000, function () {
            return reject('timeout');
        });
        return psiturk.saveData({
            error: function () {
                clearTimeout(timeout);
                console.log('Error saving data!');
                return reject('error');
            },
            success: function () {
                clearTimeout(timeout);
                console.log('Data saved to psiturk server.');
                return resolve();
            }
        });
    });
};

$(window).resize(function () {
    return checkWindowSize(800, 600, $('#jspsych-target'));
});

$(window).resize();

$(window).on('load', function () {
    var loadTimeout, slowLoad;
    // Load data and test connection to server.
    slowLoad = function () {
        var ref;
        return (ref = $('slow-load')) != null ? ref.show() : void 0;
    };
    loadTimeout = delay(12000, slowLoad);
    psiturk.preloadImages(['static/images/spider.png']);
    return delay(300, function () {
        var block, blue, color, curr_colors, i, id, orange, ref;
        console.log('Loading data');
        PARAMS = {
            inspectCost: 1,
            startTime: Date(Date.now()),
            bonusRate: .002,
            branching: '312',
            with_feedback: with_feedback,
            condition: CONDITION
        };
        psiturk.recordUnstructuredData('params', PARAMS);
        if (PARAMS.variance) {
            id = `${PARAMS.branching}_${PARAMS.variance}`;
        } else {
            id = `${PARAMS.branching}`;
        }

        STRUCTURE_TRAINING = loadJson("static/json/structure/31123.json");
        TRIALS_TRAINING = loadJson("static/json/transfer/31123_increasing32.json");
        console.log(`loaded ${(TRIALS_TRANSFER != null ? TRIALS_TRANSFER.length : void 0)} transfer trials`);

        getTrainingTrials = (function () {
            console.log('tr');
            var idx, t;
            t = _.shuffle(TRIALS_TRAINING);
            idx = 0;
            return function (n) {
                idx += n;
                return t.slice(idx - n, idx);
            };
        })();
        getTransferTrials = (function () {
            console.log('transfi');
            var idx, t;
            t = _.shuffle(TRIALS_TRANSFER);
            idx = 0;
            return function (n) {
                idx += n;
                return t.slice(idx - n, idx);
            };
        })();


        if (DEBUG || TALK) {
            createStartButton();
            return clearTimeout(loadTimeout);
        } else {
            console.log('Testing saveData');
            if (DEMO) {
                clearTimeout(loadTimeout);
                return delay(500, createStartButton);
            } else {
                return saveData().then(function () {
                    clearTimeout(loadTimeout);
                    return delay(500, createStartButton);
                }).catch(function () {
                    clearTimeout(loadTimeout);
                    return $('#data-error').show();
                });
            }
        }
    });
});

createStartButton = function () {
    if (DEBUG || TALK) {
        initializeExperiment();
        return;
    }
    if (DEMO) {
        $('#jspsych-target').append("<div class='alert alert-info'>\n  <h3>Demo mode</h3>\n\n  To go through the task as if you were a participant,\n  click <b>Begin</b> above.<br>\n  To view replays of the participants\n  in our study, click <b>View Replays</b> below.\n</div>\n<div class='center'>\n  <button class='btn btn-primary btn-lg centered' id=\"view-replays\">View Replays</button>\n</div>");
        $('#view-replays').click(function () {
            SHOW_PARTICIPANT = true;
            DEMO_TRIALS = _.shuffle(loadJson("static/json/demo/312.json"));
            return initializeExperiment();
        });
    }
    $('#load-icon').hide();
    $('#slow-load').hide();
    $('#success-load').show();
    return $('#load-btn').click(initializeExperiment);
};

initializeExperiment = function () {
    var Block, ButtonBlock, ControlBlock, MouselabBlock, QuizLoop, TextBlock, block_num, bonus_text, crt, curr_block,
        divider,
        divider_intro_training, divider_training_test, experiment_timeline, explain_control, finish, fullMessage, i,
        img, instruct_loop,
        instructions, intro0, intro1, intro2, intro3, intro_quest, mouselab_trials, ncs, nodeValuesDescription,
        num_taining_trials,
        post_test, prompt_resubmit, quiz, ref, reflection_prompt_cfbc, reflection_prompt_cfbo, reflection_prompt_cffc,
        reflection_prompt_cffo,
        reflection_prompt_efbc, reflection_prompt_efbo, reflection_prompt_effc, reflection_prompt_effo,
        control_question_1, reprompt, reset_score,
        save_data, start_trial, survey, talk_demo, test_block_intro, text, training, training_trials,
        transfer_block_intro, verbal_responses;
    $('#jspsych-target').html('');
    console.log('INITIALIZE EXPERIMENT');
    //  ======================== #
    //  ========= TEXT ========= #
    //  ======================== #

    // These functions will be executed by the jspsych plugin that
    // they are passed to. String interpolation will use the values
    // of global variables defined in this file at the time the function
    // is called.
    text = {
        debug: function () {
            if (DEBUG) {
                return "`DEBUG`";
            } else {
                return '';
            }
        }
    };
    // ================================= #
    // ========= BLOCK CLASSES ========= #
    // ================================= #
    Block = class Block {
        constructor(config) {
            _.extend(this, config);
            this._block = this; // allows trial to access its containing block for tracking state
            if (this._init != null) {
                this._init();
            }
        }

    };
    TextBlock = (function () {
        class TextBlock extends Block {
        };

        TextBlock.prototype.type = 'text';

        TextBlock.prototype.cont_key = ['space'];

        return TextBlock;

    }).call(this);
    ButtonBlock = (function () {
        class ButtonBlock extends Block {
        };

        ButtonBlock.prototype.type = 'button-response';

        ButtonBlock.prototype.is_html = true;

        ButtonBlock.prototype.choices = ['Continue'];

        ButtonBlock.prototype.button_html = '<button class="btn btn-primary btn-lg">%choice%</button>';

        return ButtonBlock;

    }).call(this);
    QuizLoop = class QuizLoop extends Block {
        loop_function(data) {
            var c, i, len, ref;
            console.log('data', data);
            ref = data[data.length].correct;
            for (i = 0, len = ref.length; i < len; i++) {
                c = ref[i];
                if (!c) {
                    return true;
                }
            }
            return false;
        }

    };
    MouselabBlock = (function () {
        class MouselabBlock extends Block {
        };

        MouselabBlock.prototype.type = 'mouselab-mdp';

        MouselabBlock.prototype.playerImage = 'static/images/spider.png';

        MouselabBlock.prototype.lowerMessage = "<b>Clicking on a node reveals its value for a $1 fee.<br>\nMove with the arrow keys.</b>";

        return MouselabBlock;

    }).call(this);
    // ControlBlock contains the colour trial
    ControlBlock = (function () {
        class ControlBlock extends Block {
        };

        ControlBlock.prototype.type = 'single-stim';

        ControlBlock.prototype.choices = ['f', 'j'];

        ControlBlock.prototype.response_ends_trial = true;

        ControlBlock.prototype.prompt = "<center>Please press <b><font color='blue'>F</font></b> or <b><font color='orange'>J</font></b>!</center>";

        return ControlBlock;

    }).call(this);



    //  ============================== #
    //  ========= EXPERIMENT ========= #
    //  ============================== #
    img = function (name) {
        return `<img class='display' src='static/images/${name}.png'/>`;
    };



    // ----------------- WELCOME ------------------------------------------------------------
    welcome = new Block({
      type: 'instructions',
      show_clickable_nav: true,
      pages: function () {
          return [markdown("<h1> Web of Cash </h1>\n\n In this experiment, you will play a game called *Web of Cash*. You will guide a\n  money-loving spider through a spider web. \n\n This experiment has two phases: \n\n- **Questionnaire:** In this phase, we will ask you to fill two questionnaires. \n\n- **Gameplay:** In this phase, you will play the *Web of Cash*. \n\nIf you complete the experiment, you will receive a base pay of " +PAY_BASE+ " and a bonus which is dependent on your performance in the game phase. On average, a person who conscientiously follows the instructions receives " +PAY_MEAN+ ".\n\n Click **Next** to start the **Questionnaire** part.")]
        }
    });

    // ----------------- QUESTIONNAIRE ------------------------------------------------------------
    ncs = new Block({
        preamble: function () {
            return markdown("# Questionnaire\n\nFor each of the statements below, please indicate to what extent the statement is characteristic of you or of what you\nbelieve. The * marks mandatory questions.");
        },
        type: 'survey-multi-choice',
        questions: ["I prefer complex to simple problems.", "I like to have the responsibility of handling a situation that requires a lot of thinking.", "Thinking is not my idea of fun.", "I would rather do something that requires little thought than something that is sure to challenge my abilities.", "I try to anticipate and avoid situations where there is a likely chance I will have to think in depth about someting.", "I find satisfaction in deliberating hard and for long hours.", "I only think as hard as I have to.", "I prefer to think about small daily projects to long term ones.", "I like tasks that require little thought once I've learned them.", "The idea of relying on thought to make my way to the top appeals to me.", "I really enjoy a task that involves coming up with new solutions to problems.", "Learning new ways to think doesn't excite me very much.", "I prefer my life to be filled with puzzles I must solve.", "The notion of thinking abstractly is appealing to me.", "I would prefer a task that is intellectual, difficult, and important to one that is somewhat important but does not require much thought.", "I feel relief rather than satisfaction after completing a task that requires a lot of mental effort.", "It's enough for me that something gets the job done; I don't care how or why it works.", "I usually end up deliberating about issues even when they do not affect me personally."],
        options: [['1 = extremely uncharacteristic of me', '2 = somewhat uncharacteristic of me', '3 = uncertain', '4 = somewhat characteristic of me', '5 = extremely characteristic of me'], ['1 = extremely uncharacteristic of me', '2 = somewhat uncharacteristic of me', '3 = uncertain', '4 = somewhat characteristic of me', '5 = extremely characteristic of me'], ['1 = extremely uncharacteristic of me', '2 = somewhat uncharacteristic of me', '3 = uncertain', '4 = somewhat characteristic of me', '5 = extremely characteristic of me'], ['1 = extremely uncharacteristic of me', '2 = somewhat uncharacteristic of me', '3 = uncertain', '4 = somewhat characteristic of me', '5 = extremely characteristic of me'], ['1 = extremely uncharacteristic of me', '2 = somewhat uncharacteristic of me', '3 = uncertain', '4 = somewhat characteristic of me', '5 = extremely characteristic of me'], ['1 = extremely uncharacteristic of me', '2 = somewhat uncharacteristic of me', '3 = uncertain', '4 = somewhat characteristic of me', '5 = extremely characteristic of me'], ['1 = extremely uncharacteristic of me', '2 = somewhat uncharacteristic of me', '3 = uncertain', '4 = somewhat characteristic of me', '5 = extremely characteristic of me'], ['1 = extremely uncharacteristic of me', '2 = somewhat uncharacteristic of me', '3 = uncertain', '4 = somewhat characteristic of me', '5 = extremely characteristic of me'], ['1 = extremely uncharacteristic of me', '2 = somewhat uncharacteristic of me', '3 = uncertain', '4 = somewhat characteristic of me', '5 = extremely characteristic of me'], ['1 = extremely uncharacteristic of me', '2 = somewhat uncharacteristic of me', '3 = uncertain', '4 = somewhat characteristic of me', '5 = extremely characteristic of me'], ['1 = extremely uncharacteristic of me', '2 = somewhat uncharacteristic of me', '3 = uncertain', '4 = somewhat characteristic of me', '5 = extremely characteristic of me'], ['1 = extremely uncharacteristic of me', '2 = somewhat uncharacteristic of me', '3 = uncertain', '4 = somewhat characteristic of me', '5 = extremely characteristic of me'], ['1 = extremely uncharacteristic of me', '2 = somewhat uncharacteristic of me', '3 = uncertain', '4 = somewhat characteristic of me', '5 = extremely characteristic of me'], ['1 = extremely uncharacteristic of me', '2 = somewhat uncharacteristic of me', '3 = uncertain', '4 = somewhat characteristic of me', '5 = extremely characteristic of me'], ['1 = extremely uncharacteristic of me', '2 = somewhat uncharacteristic of me', '3 = uncertain', '4 = somewhat characteristic of me', '5 = extremely characteristic of me'], ['1 = extremely uncharacteristic of me', '2 = somewhat uncharacteristic of me', '3 = uncertain', '4 = somewhat characteristic of me', '5 = extremely characteristic of me'], ['1 = extremely uncharacteristic of me', '2 = somewhat uncharacteristic of me', '3 = uncertain', '4 = somewhat characteristic of me', '5 = extremely characteristic of me'], ['1 = extremely uncharacteristic of me', '2 = somewhat uncharacteristic of me', '3 = uncertain', '4 = somewhat characteristic of me', '5 = extremely characteristic of me']],
        required: DEBUG ? [false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false] : [true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true],
        bold: true,
        button: 'Continue'
    });
    quest_block = new Block({
      timeline: [welcome, ncs]
    });

    // ----------------- INSTRUCTIONS ------------------------------------------------------------
    instructions_mouselab = new Block({
        type: 'instructions',
        show_clickable_nav: true,
        pages: function () {
            return [markdown(" <h1> Web of Cash </h1>\n\n In this experiment, you will play a game called *Web of Cash*. You will guide a\n money-loving spider through a spider web. " +
                "When you land on a gray circle\n (a ***node***) the value of the node is added to your score.\n\n You will be able to move the spider with the arrow keys, " +
                "but only in the direction\n of the arrows between the nodes. The image below shows the web that you will be navigating when the game starts." +
                "\n\n<img class='display' style=\"width:65%; height:auto\" src='static/images/web-of-cash-unrevealed-large.png'/>\n"),
                markdown("## Node Inspector\n\nIt's hard to make good decision when you can't see what you will get!\nFortunately, " +
                    "you will have access to a ***node inspector*** which can reveal\nthe value of a node. To use the node inspector, " +
                    "simply ***click on a node***. The image below illustrates how this works.\n\n**Note:** You can only use the node inspector when you're on the first\nnode." +
                    "\n\n<img class='display' style=\"width:65%; height:auto\" src='static/images/web-of-cash-large.png'/>\n\n"),
                markdown("## Rewards and Costs\n- Each node of the web either contains a <b><font color='green'>reward</font></b> or a <b><font color='red'>loss</font></b>" +
                    "\n- You can find out about a node's loss or reward by using the node inspector.\n- The fee for using the node inspector is <b>$1 per click</b>.\n\n"),
                markdown("## Additional Information\n\n<img class='display' style=\"width:65%; height:auto\" src='static/images/web-of-cash-large.png'/>\n- Every time you play this game the rewards on the web will be different. " +
                    "So you have to make a new plan every time.\n"),
                markdown(`## Practice makes perfect\n\n- The better you perform in the game, the higher your bonus will be.
            \n${(with_feedback ? "- An intelligent tutor will give you helpful feedback on how you decide what to do." : "")}
            \n`), markdown("## Quiz\n\nBefore you can begin playing *Web of Cash*, you must pass a quiz to show\nthat you understand the rules. If you get any of the questions" +
                    "\nincorrect, you will be brought back to the instructions to review and\ntry the quiz again.")];
        }
    });


    quiz_mouselab = new Block({
        preamble: function () {
            return markdown(`<h1>Quiz</h1>Please answer the following questions about the *Web of Cash*. Questions marked with (*) are compulsory. <br><br>
              <button id="quizInfoButton" class="btn btn-primary" onclick="getQuizInfo()">
                Show instructions</button> <div id="quizInfoDiv" style="width:800px;text-align:left;margin: 0 auto;"></div>`);
        },
        type: 'survey-multi-choice',
        questions: ["How can you learn the value of a node?", "Will you receive a bonus?", "Will each round be the same?", "What is the cost of clicking on a node to find out its value?"],
        options: [['By hitting enter on your keyboard.', 'By clicking on the node with your mouse cursor.', 'There is no option to do so.'], ['No.', 'I will receive a £1 bonus regardless of my performance.', 'I will only receive a £1 bonus if I perform well, otherwise I will receive no bonus.', 'The better I perform the higher my bonus will be.'], ['Yes.', 'No, the amount of cash at each node of the web may be different each time.', 'No, the structure of the web will be different each time.', 'No, the cost for clicking on a node will be different each time.'], ['$0', '$1', '$5', '$10']],
        required: DEBUG ? [false, false, false, false, false] : [true, true, true, true, true],
        bold: true,
        correct: ['By clicking on the node with your mouse cursor.', 'The better I perform the higher my bonus will be.', 'No, the amount of cash at each node of the web may be different each time.',  '$1']
    });
    instructions_mouselab_loop = new Block({
        timeline: [instructions_mouselab, quiz_mouselab],
        loop_function: function (data) {
            var c, i, len, ref;
            ref = data[1].correct;
            for (i = 0, len = ref.length; i < len; i++) {
                c = ref[i];
                if (!c && !DEBUG) {
                      REPETITIONS += 1;
                      if(REPETITIONS >= 3){
                        psiturk.recordUnstructuredData('exclusion', true);
                        psiturk.saveData();
                        $('#jspsych-target').html(`<h1>Please return your submission</h1>\n<p>\nThank you for your interest in this study.
                          However, it is crucial that you understand these instructions to take part, and our data indicate that this is not the case.
                          I'm afraid you cannot proceed further in the experiment as a result.
                          Please return your submission on Prolific now by selecting the "Stop without completing" button, to avoid receiving a rejection.`);
                      }
                      else{
                        console.log('wrong way2');
                        alert(`You got at least one question wrong. We'll send you back to the instructions and then you can try again.`);
                        return true; // try again
                  }
                }
            }
            psiturk.recordUnstructuredData('exclusion', false);
            psiturk.finishInstructions();
            psiturk.saveData();
            return false;
        }
    });



    // ----------------- TRAINING ------------------------------------------------------------
    intro_mouselab_block = new Block({
        type: 'instructions',
        show_clickable_nav: true,
        pages: function () {
            SCORE = 0;
            return [markdown(" <h1> Congratulations! </h1>\n\n You have completed the instructions block."),
          markdown("<h1> Web of Cash </h1>\n\n In the following 21 trials, you will have the chance to play *Web of Cash*. At every third trial, we will give you a short break where a small questionnaire will be waiting for you. You can earn a bonus that depends on your performance. Concretely, you will earn 2 additional cents for every $10 you make in the game. For example, if your final score is $500, you will receive a bonus of £1. The maximum bonus is " +PAY_MAX+ " <br/>\n Good luck!\n")];
        }
    });

    intro_mouselab_block_controlPassive = new Block({
        type: 'instructions',
        show_clickable_nav: true,
        pages: function () {
            SCORE = 0;
            return [markdown(" <h1> Congratulations! </h1>\n\n You have completed the instructions block."),
          markdown("<h1> Web of Cash </h1>\n\n In the following 21 trials, you will have the chance to play *Web of Cash*. You can earn a bonus that depends on your performance. Concretely, you will earn 2 additional cents for every $10 you make in the game. For example, if your final score is $500, you will receive a bonus of £1. The maximum bonus is " + PAY_MAX + " .<br/>\n Good luck!\n")];
        }
    });

    // --- new reflection prompts !!!!

    getTrialsBlock = function(trialCount){
      return new MouselabBlock({
          minTime: DEBUG ? 1 : 7,
          show_feedback: with_feedback,
          revealRewards: reveal_rewards,
          blockName: 'training',
          stateDisplay: 'click',
          stateClickCost: PARAMS.inspectCost,
          startScore: TRAINING_SCORE,

          timeline: getTrainingTrials(prompt_length),
          _init: function () {
              _.extend(this, STRUCTURE_TRAINING);
              this.round_length = 7*prompt_length;
              return this.trialCount = trialCount;
          }
      });
    }

    divider_prompts = new TextBlock({
        cont_key: [32],
        text: function () {
            return "<h1>Reflection</h1> Please reflect about your planning success in the last three rounds by answering a couple of questions. <br><br>Press <code>space</code> to continue."
        }
    });

    getFeedbackTrial = function(trialCount){
      return new TextBlock({
          cont_key: [45],
          text: function () {

              // upate values
              RECENT_SCORE2 = RECENT_SCORE1;
              RECENT_SCORE1 = SCORE - RECENT_SCORE3;

              // calculate average
              let av1 = Math.round(RECENT_SCORE1/prompt_length);
              let av2 = Math.round(RECENT_SCORE2/prompt_length);
              let av3 = Math.round(RECENT_SCORE3/Math.max(1,trialCount));

              // update again
              RECENT_SCORE3 = SCORE;

              let redGreen = function(val) {
                if (val > 0) {
                  return '#080';
                } else if (val < 0) {
                  return '#b00';
                } else {
                  return '#666';
                }
              };

              let text = "<p style='font-size:17pt'>Your average score in the <b>last three rounds</b> was <span style='color:" + redGreen(av1) +"'><b>" + av1 + "</b></span>"

              if(trialCount > 0){
                text = text + "<br><br>Your average score in the <b>three rounds before that</b> was <span style='color:" + redGreen(av2) +"'><b>" + av2 + "</b></span>."
              }

              setTimeout(function(){$('#jspsych-target').html(''); jsPsych.finishTrial();}, 4000);
              return text;
          }
      });
    }

    getReflectionBlock = function(trialCount){

      // prompt questions
      let questions = ['How many clicks did you make to decide what to do?', 'Where did you click in order to decide what to do?', 'Did you use any particular strategy for selecting your clicks? If so, what was it?', 'How well do you think your current strategy is working?', 'Why do you think it worked out that way?', 'Based on what you have learned in the last rounds, what tip could you give to a person who performs this task for the first time?', 'Based on the previous questions, how many clicks do you plan to do in the next rounds?', 'Based on the previous questions, where do you plan to click in the next rounds?',  'Which strategy do you want to use to select your clicks in the next rounds?'];

      let res = new Block({
        timeline: [getTrialsBlock(trialCount), divider_prompts, getFeedbackTrial(trialCount)]
      });

      for(q in questions){
        let prompt = null;

        if(q == 3){
          prompt = new Block({
              preamble: markdown("<p style='text-align: left; font-size: 15pt'>Question "+ String(Number(q)+1) + "/9 </p> <div><h3>Please answer the question:</h3></div> "),
              type: 'survey-multi-choice',
              options: [["Very Well", "Well", "Acceptable",  "Poor", "Very Poor"]],
              questions: [questions[q]],
              required: [true],
              bold: true,
              button: 'Continue'
          });

        } else {
          prompt = new Block({
              type: DEBUG ? 'survey-text' : 'survey-text-validate',
              preamble: markdown("<p style='text-align: left; font-size: 15pt'>Question "+ String(Number(q)+1) + "/9 </p> <div><h3>Please answer the question:</h3></div> "),
              questions: [questions[q]],
              button: 'Continue'
          });
        }

        res.timeline.push(prompt)
      }

      return res;
    }

    mouselab_block_reflection = new Block({
            timeline: [intro_mouselab_block, getReflectionBlock(0), getReflectionBlock(prompt_length*1), getReflectionBlock(prompt_length*2), getReflectionBlock(prompt_length*3), getReflectionBlock(prompt_length*4), getReflectionBlock(prompt_length*5), getTrialsBlock(prompt_length*6)]
          });


    // ---- new control block !
    divider_control = new TextBlock({
        cont_key: [32],
        text: function () {
            return "<div style='text-align: center; font-size: 19pt'> Press <code>space</code> to continue.</div>";
        }
    });

    getControlBlock = function(index){

      // prompt questions
      let topics = ['TV show', ' food', 'book', 'season', ' vacation', ' sports', 'movie', ' transport', 'free-time activity'];
      let questions = ['Please describe your favorite ', 'What do you like about it? (3-4 sentences)', 'What do you dislike about it? (3-4 sentences)'];

      let res = new Block({
        timeline: [getTrialsBlock(prompt_length*index), divider_control, getFeedbackTrial(index)]
      });

      for(q in questions){
        let prompt = new Block({
            type: DEBUG ? 'survey-text' : 'survey-text-validate',
            preamble: markdown("<p style='text-align: left; font-size: 15pt'>Question "+ String(Number(q)+1) + "/3 </p> <div><h3>Please complete the question:</h3></div> "),
            questions: q == 0 ? [questions[q] + topics[index] + '? (3-4 sentences)']: [questions[q]],
            button: 'Continue'
        });

        res.timeline.push(prompt)
      }

      return res;
    }

    mouselab_block_controlActive = new Block({
            timeline: [intro_mouselab_block, getControlBlock(0), getControlBlock(1), getControlBlock(2), getControlBlock(3), getControlBlock(4), getControlBlock(5), getTrialsBlock(prompt_length*6)]
          });

    mouselab_block_controlPassive = new Block({
            timeline: [intro_mouselab_block_controlPassive, getTrialsBlock(prompt_length*0), getTrialsBlock(prompt_length*1), getTrialsBlock(prompt_length*2), getTrialsBlock(prompt_length*3), getTrialsBlock(prompt_length*4), getTrialsBlock(prompt_length*5), getTrialsBlock(prompt_length*6)]
    });

    // ----------------- FINISH  ------------------------------------------------------------
    survey = new Block({
        type: 'survey-dropdown',
        preamble: function () {
            return markdown(`# You've almost completed the experiment.\n\nPlease briefly answer the questions below.`);
        },
        questions: ['Have you ever participated in this (Web of Cash) or a smiliar game (Tree of Cash) on Prolific or MTurk before? (We approve and pay you regardless of your answer.)', 'What is your age?', 'Which gender do you identify with?'],
        options: [['No', 'Yes'],
                   Array.from({length: 82}, (_, i) => i + 18),
                  ['Female', 'Male', 'Non-binary', 'Other']],
        button: 'Next',
        validate: true
    });
    survey_reflection = new Block({
        type: 'survey-dropdown',
        preamble: function () {
            return markdown(`# You've almost completed the experiment.\n\nPlease briefly answer the questions below.`);
        },
        questions: ['Have you ever participated in this (Web of Cash) or a smiliar game (Tree of Cash) on Prolific or MTurk before? (We approve and pay you regardless of your answer.)', 'What is your age?', 'Which gender do you identify with?', 'Since we are doing science, we would now like to know how much attention/effort you put into the reflection questions. (We approve and pay you regardless of your answer.)'],
        options: [['No', 'Yes'],
                   Array.from({length: 82}, (_, i) => i + 18),
                  ['Female', 'Male', 'Non-binary', 'Other'],
                  ['A lot of effort (e.g. you have deeply considered the questions and answered in detail)',
                   'Some effort (e.g. you have read the questions and answered them intuitively)',
                   'Minimal effort (e.g. ignoring the questions)',
                   'Unsure']],
        button: 'Next',
        validate: true
    });

    finish = new Block({
      type: 'survey-text',
      preamble: function() {
        return markdown(`# You've completed the experiment\n\n Feel free to give us feedback below before you submit the experiment.\n\n You'll be awarded a bonus based on your performance in 24 hours after the end of the experiment. \n\n Thank you for participating! Hope you enjoyed!`);
      },
      questions: ['Any comments/feedback?'],
      button: 'To the completion code'
    });

    experiment_timeline = (function () {

        let condition_branches = [mouselab_block_controlPassive, mouselab_block_controlActive, mouselab_block_reflection];
        let timeline = [quest_block, instructions_mouselab_loop];
        timeline.push(condition_branches[CONDITION]);

        timeline.push(divider_control);
        CONDITION == 2 ? timeline.push(survey_reflection) : timeline.push(survey);
        timeline.push(finish);
        return timeline;
    })();

    flatten_timeline = function(timeline){
      var global_timeline = [];

      for(var i in timeline){
        t = timeline[i];

        if(t.timeline !== undefined){
          //recursive for sub timelines
          global_timeline.push( flatten_timeline( t.timeline ));
        } else {
          // its a real block
          if(t.type !== undefined){
            info = t.type;

            if(t.questions !== undefined){
              //info = info + ' : ' + t.questions.toString();
            }
            global_timeline.push( info);

          } else if (t.trial_id !== undefined){
            global_timeline.push( 'Mouselab : ' + t.trial_id)
          }
        }
      }
      global_timeline = [global_timeline.flat(1)];
      return(global_timeline);
    }
    psiturk.recordUnstructuredData('global_timeline', JSON.stringify(flatten_timeline(experiment_timeline)) );
    //console.log(JSON.stringify(flatten_timeline(experiment_timeline)));



    // ================================================ #
    // ========= START AND END THE EXPERIMENT ========= #
    // ================================================ #

    // bonus is the total score multiplied by something
    calculateBonus = function () {
        var bonus;
        bonus = SCORE * PARAMS.bonusRate;
        bonus = (Math.round(bonus * 100)) / 100; // round to nearest cent
        bonus = Math.min(bonus, 2.5);
        return Math.max(0, bonus);
    };
    reprompt = null;
    save_data = function () {
        return psiturk.saveData({
            success: function () {
                console.log('Data saved to psiturk server.');
                if (reprompt != null) {
                    window.clearInterval(reprompt);
                }
                return psiturk.completeHIT();
            },
            error: function () {
                return prompt_resubmit;
            }
        });
    };
    prompt_resubmit = function () {
        $('#jspsych-target').html("<h1>Oops!</h1>\n<p>\nSomething went wrong submitting your HIT.\nThis might happen if you lose your internet connection.\nPress the button to resubmit.\n</p>\n<button id=\"resubmit\">Resubmit</button>");
        return $('#resubmit').click(function () {
            $('#jspsych-target').html('Trying to resubmit...');
            reprompt = window.setTimeout(prompt_resubmit, 10000);
            return save_data();
        });
    };
    return jsPsych.init({
        display_element: $('#jspsych-target'),
        timeline: experiment_timeline,
        on_finish: function () {
            if (DEBUG) {
                return jsPsych.data.displayData();
            } else {
                psiturk.recordUnstructuredData('final_bonus', calculateBonus());
                return save_data();
            }
        },
        on_data_update: function (data) {
            console.log('data', data);
            return psiturk.recordTrialData(data);
        }
    });
};