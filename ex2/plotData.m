function plotData(X, y)
%PLOTDATA Plots the data points X and y into a new figure 
%   PLOTDATA(x,y) plots the data points with + for the positive examples
%   and o for the negative examples. X is assumed to be a Mx2 matrix.

% Create New Figure
figure; hold on;

% ====================== YOUR CODE HERE ======================
% Instructions: Plot the positive and negative examples on a
%               2D plot, using the option 'k+' for the positive
%               examples and 'ko' for the negative examples.
%

m = size(X);
P = []; N = [];
for i = 1:m
	if y(i)
		P = [P [X(i, 1); X(i, 2)]];
	else
		N = [N [X(i, 1); X(i, 2)]];
	end;
end;
plot(P(1,:), P(2,:), 'k+');
plot(N(1,:), N(2,:), 'ko');

% =========================================================================



hold off;

end
