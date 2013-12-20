%% List of Nodes on which cover will be active during test.
      %% Nodes = [atom()]
      
     %%{nodes, ['test@127.0.0.1']}.       

      %% Files with previously exported cover data to include in analysis.
       %%CoverDataFiles = ["controller_recover_tcpserver"]

%%      {import, CoverDataFiles}.

      %% Cover data file to export from this session.
      %% CoverDataFile = string()

      %%{export, CoverDataFile}.

      %% Cover analysis level.
      %% Level = details | overview

      {level, details}.       

      %% Directories to include in cover.
      %% Dirs = [string()]

      {incl_dirs, ["ebin","src"]}.

      %% Directories, including subdirectories, to include.

      %%{incl_dirs_r, ["./"]}.

      %% Specific modules to include in cover.
      %% Mods = [atom()]

      %%{incl_mods,[file_utils]}.

      %% Directories to exclude in cover.

      %%{excl_dirs, Dirs}.

      %% Directories, including subdirectories, to exclude.

      %%{excl_dirs_r, Dirs}.

      %% Specific modules to exclude in cover.

      %%{excl_mods, Mods}.