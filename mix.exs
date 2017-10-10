defmodule AMOC.Mixfile do
  use Mix.Project

  def project do
    [app: :amoc,
     version: "0.9.0",
     elixir: "~> 1.4",
     start_permanent: Mix.env == :prod,
     erlc_options: erlc_options(Mix.env),
     erlc_paths: ["src", "scenarios", "deps/wocky_app/apps/wocky_xmpp/test"],
     aliases: aliases(),
     deps: deps(),
     elvis_config: elvis_config()
   ]
  end

  defp erlc_options(:test), do: [{:d, :TEST} | erlc_options(:dev)]
  defp erlc_options(_) do
    [
      :debug_info,
      :warn_missing_spec,
      :warn_export_vars,
      :warn_obsolete_guard,
      :warn_unused_import,
      {:i, "deps/wocky_app/apps/wocky_xmpp/include"},
      {:warn_format, 1},
      {:parse_transform, :lager_transform},
    ]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [description: 'A Murder Of Crows XMPP load tester for Wocky',
     extra_applications: [:wocky, :jiffy],
     mod: {:amoc_app, []},

     env: [repeat_interval: 60000,
           interarrival: 30,
           exometer_predefined: [
             {[:amoc, :users],
               {:function, :ets, :info, [:amoc_users], :proplist, [:size]},
               []},
             {[:amoc, :times, :connection], :histogram, []},
             {[:amoc, :counters, :connections], :spiral, []},
             {[:amoc, :counters, :connection_failures], :spiral, []}
           ]
     ]
     ]
  end

  defp deps do
    [
      {:distillery, "~> 1.1", runtime: false},

      {:lager,
        github: "basho/lager",
        tag: "3.2.1",
        manager: :rebar3,
        override: true},
      {:jiffy,
        github: "davisp/jiffy",
        tag: "0.14.7",
        manager: :rebar,
        override: true},
      {:trails,
        github: "inaka/cowboy-trails",
        tag: "0.1.1",
        manager: :rebar3,
        override: true},
      {:cowboy_swagger,
        github: "inaka/cowboy-swagger",
        tag: "1.0.3",
        manager: :rebar3},
      {:escalus,
        github: "esl/escalus",
        ref: "47848b5",
        manager: :rebar3,
        override: true},
      {:usec,
        github: "esl/usec",
        branch: "master",
        manager: :rebar3,
        override: true},
      {:exometer,
        github: "Feuerlabs/exometer",
        ref: "7a7bd8d2b52de4d90f65aa3f6044b0e988319b9e",
        manager: :rebar3,
        override: true},
      {:lhttpc,
        github: "esl/lhttpc",
        branch: "otp-17-compat",
        manager: :rebar3},
      {:mochijson2,
        github: "bjnortier/mochijson2",
        branch: "master",
        manager: :rebar3,
        override: true,
        runtime: false
      },
      {:proper,
        github: "manopapad/proper",
        branch: "master",
        manager: :rebar3,
        override: true},
      {:recon,
        github: "ferd/recon",
        tag: "2.2.1",
        manager: :rebar3,
        override: true},
      {:cowboy,
        github: "ninenines/cowboy",
        tag: "1.0.4",
        manager: :rebar3,
        override: true},
      {:fusco,
        github: "esl/fusco",
        manager: :rebar3,
        override: true},
      {:wocky_app,
        git: "git@github.com:hippware/wocky.git",
        branch: "amoc-additions",
        runtime: false
      },


      # Overrides to get us compiling with wocky
      {:exml,
        github: "esl/exml",
        ref: "d365533",
        manager: :rebar3,
        override: true},
      {:exometer_core,
        github: "Feuerlabs/exometer_core",
        tag: "1.4",
        manager: :rebar3,
        override: true},
      {:mustache,
        github: "mojombo/mustache.erl",
        branch: "master",
        manager: :rebar3,
        override: true},
      {:wsecli,
        github: "esl/wsecli",
        branch: "master",
        override: true},
      {:amqp_client,
        github: "dsrosario/amqp_client",
        branch: "erlang_otp_19",
        override: true},
      {:base16, "~> 1.0", override: true},
      {:ranch, "~> 1.0.0", override: true},
      {:meck, "~> 0.8.7", override: true},
      {:folsom, "~> 0.8.3", override: true},
      {:edown, "~> 0.8.1", override: true},

    ]
  end

  defp aliases do
    [
    ]
  end

  defp elvis_config do
    [
      %{dirs: ['src'],
        filter: '*.erl',
        rules: [
          {:elvis_style, :line_length,
           %{limit: 80, skip_comments: false}},
          {:elvis_style, :no_tabs},
          {:elvis_style, :no_trailing_whitespace},
          # We use macros for Elixir module names
          # {:elvis_style, :macro_names},
          # {:elvis_style, :macro_module_names},
          {:elvis_style, :operator_spaces,
           %{rules: [right: ",", right: "++", left: "++"]}},
          {:elvis_style, :nesting_level, %{level: 3}},
          {:elvis_style, :god_modules, %{limit: 25, ignore: []}},
          {:elvis_style, :no_if_expression},
          {:elvis_style, :invalid_dynamic_call},
          {:elvis_style, :used_ignored_variable},
          {:elvis_style, :no_behavior_info},
          {:elvis_style, :module_naming_convention,
           %{regex: "^[a-z]([a-z0-9]*_?)*(_SUITE)?$"}},
          {:elvis_style, :function_naming_convention,
           %{regex: "^([a-z][a-z0-9]*_?)*|'>>='$"}},
          {:elvis_style, :state_record_and_type},
          {:elvis_style, :no_spec_with_records},
          {:elvis_style, :dont_repeat_yourself, %{min_complexity: 15}},
          {:elvis_style, :no_debug_call},
          {:elvis_style, :variable_naming_convention,
           %{regex: "^(_?[A-Z][0-9a-zA-Z]*)$"}}
        ]},

      # Slightly less strict rules for tests
      %{dirs: ['test'],
        filter: '*.erl',
        rules: [
          {:elvis_style, :line_length,
           %{limit: 80, skip_comments: false}},
          {:elvis_style, :no_tabs},
          {:elvis_style, :no_trailing_whitespace},
          # We use macros for Elixir module names
          # {:elvis_style, :macro_names},
          # {:elvis_style, :macro_module_names},
          {:elvis_style, :operator_spaces,
           %{rules: [right: ",", right: "++", left: "++"]}},
          {:elvis_style, :nesting_level, %{level: 3}},
          {:elvis_style, :god_modules, %{limit: 25, ignore: [:mam_SUITE,
                                                             :test_helper]}},
          {:elvis_style, :no_if_expression},
          {:elvis_style, :invalid_dynamic_call},
          {:elvis_style, :used_ignored_variable},
          {:elvis_style, :no_behavior_info},
          {:elvis_style, :module_naming_convention,
           %{regex: "^[a-z]([a-z0-9]*_?)*(_SUITE)?$"}},
          {:elvis_style, :function_naming_convention,
           %{regex: "^([a-z][a-z0-9]*_?)*|'>>='$"}},
          {:elvis_style, :state_record_and_type},
          {:elvis_style, :no_spec_with_records},
          {:elvis_style, :dont_repeat_yourself, %{min_complexity: 20}},
          {:elvis_style, :no_debug_call, %{ignore: [:mam_SUITE]}},
          {:elvis_style, :variable_naming_convention,
           %{regex: "^(_?_?[A-Z][0-9a-zA-Z]*)$"}}
        ]}
    ]
  end
end
