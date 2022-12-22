-record(monkey, {id,
                 items = [],
                 operator,
                 operand,
                 test,
                 recipient_for_true,
                 recipient_for_false,
                 inspected = 0,
                 worry_level_postproc
                }).
