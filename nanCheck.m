function nanCheck(fil)
% check if NaN value contained   
check = isnan(fil);
if(check == zeros(size(check)))
disp('Well Done')
else
disp('Ah Error')
endif
