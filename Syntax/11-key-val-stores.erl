

% For small amounts of data, there are basically two data structures that can be used.

%% The first one is called a proplist. A proplist is any list of tuples of the form [{Key,Value}].

%% If you do want a more complete key-value store for small amounts of data, the orddict module is what you need.
%% Orddicts (ordered dictionaries) are proplists with a taste for formality.
%% Each key can be there once, the whole list is sorted for faster average lookup, etc.
%% Orddicts are a generally good compromise between complexity and efficiency up to about 75 elements

%% There are basically two key-value structures/modules to deal with larger amounts of data: dicts and gb_trees. 