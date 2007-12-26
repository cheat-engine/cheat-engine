# Generate an .IMP file suitable for UC to self-import UCRI
BEGIN { print "UC2 GNU" }
/XTrace|XEntry|XFunction|XType|XNTable|XClass|XModule|XTemplateFun|uc_global|uc_std|uc_eval_method|uc_ucri_init|ucri_instruction_counter/ { if ($0 !~ /_Rb_tree/ && $1 != "U") print $1,substr($3,2,length($3)) }

