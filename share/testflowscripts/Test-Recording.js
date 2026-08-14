var Home = require("steps/Home.js");
var Project = require("Audacity.Project");
var u = require("steps/TestUtils.js");

const RECORDING_DURATION_MS = 150;
const MINIMUM_EXPECTED_DURATION_SECONDS = 0.05;
const TRIAL_COUNT = 3;

var results = [];
var failures = 0;

function performShortRecording(trial) {
    var firstNewTrackIndex = Project.trackCount();

    api.log.info(
        "Trial " +
            trial +
            ": starting " +
            RECORDING_DURATION_MS +
            " ms recording",
    );

    // This creates and selects a new track before starting recording.
    u.run("record-on-new-track");

    // Keep this below mMinCaptureSecsToCopy, which is 200 ms.
    u.sleep(RECORDING_DURATION_MS);

    u.run("action://record/stop");

    // Allow recording finalization and project notifications to complete.
    u.sleep(750);

    const clip = Project.clipsOnTrack(Project.trackCount() - 1)[0];
    const duration = clip.end - clip.start;
    results.push(duration);

    api.log.info(
        "Trial " +
            trial +
            ": clip duration = " +
            duration.toFixed(6) +
            " seconds",
    );

    if (duration < MINIMUM_EXPECTED_DURATION_SECONDS) {
        ++failures;
        api.log.error(
            "Trial " +
                trial +
                ": recording is empty or too short; final capture buffer " +
                "was probably not drained",
        );
    }
}

var testCase = {
    name: "TC3.1: Recording final capture-buffer drain",
    description:
        "Checks that a sub-200 ms recording is committed when recording stops",

    steps: [
        {
            name: "Create a clean project",
            func: function () {
                u.run("file-close");
                u.sleep(300);

                Home.goToHome();
                u.sleep(300);

                Home.createNewProject();
                u.sleep(1000);
            },
        },

        {
            name: "Make short recordings",
            func: function () {
                for (var trial = 1; trial <= TRIAL_COUNT; ++trial) {
                    performShortRecording(trial);
                }
            },
        },

        {
            name: "Verify recorded durations",
            func: function () {
                var summary = [];

                for (var i = 0; i < results.length; ++i) {
                    summary.push(results[i].toFixed(6));
                }

                api.log.info(
                    "Recorded durations: [" + summary.join(", ") + "]",
                );

                if (failures > 0) {
                    u.fail(
                        failures +
                            " of " +
                            TRIAL_COUNT +
                            " short recordings contained less than " +
                            MINIMUM_EXPECTED_DURATION_SECONDS.toFixed(3) +
                            " seconds of audio",
                    );
                }
            },
        },
    ],
};

function main() {
    api.testflow.setInterval(250);
    api.testflow.runTestCase(testCase);
}
