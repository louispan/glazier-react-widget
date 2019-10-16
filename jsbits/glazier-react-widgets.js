var hgrw$InputComponent_ = null;
function hgr$InputComponent() {
    if (!hgrw$InputComponent_) {
        const ReactPureComponent = hgr$React()["PureComponent"];
        const diff = require('diff');

        // used for calculating new start
        // Basically as we count down old start, we count up new start
        // stops when original start is zero
        const stepSelection = function(diff, old, n) {
            // data only in after:
            // if orig start < 0
            // before: foo|b*r
            // after : foo|b*ar
            // new start is < 0 -> no change
            // old start doesn't change (old string doesn't have this char to increment index)

            // if origin start > 0
            // before: foob*|r
            // after : foob*a|r
            // increment new start -> move past extra char
            // old start does't change (old string doesn't have this char to increment index)
            if (diff.added) {
                return [old, old > 0 ? n + diff.value.length : n];
            }
            // data only in before:
            // if orig start < 0
            // before: foo|b*ar
            // after : foo|b*r
            // old start is < 0 -> no change
            // new start doesn't change (new string doesn't have this char to increment index)
            //
            // if origin start > 0
            // before: foob*a|r
            // after : foob*|r
            // decrement old start -> (move past extra char)
            // new start does't change (new string doesn't have this char to increment index)
            else if (diff.removed) {
                return [old > 0 ? old - diff.value.length : 0, n];
            }
            // common data: lockstep - we want to decrement old start and increment new start
            // as long as original start is above zero
            else {
                if (old > 0)diff.value.length
                    return [old - diff.value.length, n + diff.value.length]
                else
                    return [0, n]
            }
        };

        // checkbox intermediate state is not settable by HTML
        // so this wrapper makes it settable.

        // Also, in GHJS, text inputs doesn't interact well as a React controlled component.
        // Eg. cursor jumps if user types quickly.

        // It is because there is a race condition with lazy event handlers setting the value,
        // So this prototype uses the React uncontrolled component
        // (using defaultValue instead of value).

        // For <input>, React uses controlled input if input.value is not null
        // and there is an onChange handler.

        // Vanilla React uncontrolled input only reads "defaultValue" once.

        // On rerender, thi widget will read "props.value", and set the DOM input value in javascript.
        // So, try to avoid triggering a re-render unless absolutely necessary
        // In normal cases, you don't need to trigger a rerender since it is an
        // uncontrolled component, and will be rerendered automatically by the DOM
        // from user input.

        // This widget attempts to set the cursor position at the correct place
        // by using a diffing algorithm on the old and new value.
        class InputComponent extends ReactPureComponent {

            constructor(props) {
                super(props);
                this.state = {};
                this.setInput = this.setInput.bind(this);
                this.input = null;
            }

            updateInput() {
                if (this.input) {
                    // safe if this.props.indeterminate is undefined
                    this.input.indeterminate = this.props.indeterminate
                    // Using uncontrolled input
                    // input stringifies "undefined" but converts null to ''
                    if (typeof this.props.value !== 'undefined') {
                        this.input.value = this.props.value;

                        // first calculate the new selection rang
                        var s0 = this.input.selectionStart;
                        var e0 = this.input.selectionEnd;
                        const ds = diff.diffChars(this.input.value, this.props.value, {ignoreCase: true});
                        var s1 = 0;
                        var e1 = 0;
                        for (d of ds) {
                            if (s0 <= 0 && e0 <= 0)
                                break;
                            [s0, s1] = stepSelection(d, s0, s1);
                            [e0, e1] = stepSelection(d, e0, e1);
                        }
                        this.input.setSelectionRange(s1, e1);
                    }
                    else {
                        this.input.value = '';
                    }
                }
            }

            setInput(input) {
                this.input = input;
                updateInput();
                // also forward to ref prop if set
                if (this.props.ref)
                    this.props.ref(input);
            }

            componentDidUpdate(prevProps, prevState) {
                updateInput();
            }

            render() {
                // hide props that React doesn't know about or will make input into a controlled value
                props = hgr$objectWithoutProperties(this.props, ["indeterminate", "ref", "value"]);
                props.ref = this.setInput;
                props.defaultValue = this.props.value; // this only works on the first render
                return React.createElement("input", props);
            }
        }
        hgrw$InputComponent_ = InputComponent;
    }
    return hgrw$InputComponent_;
}
