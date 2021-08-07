# rebar3 provider sets this to all deps of the project being built
paths = String.split(System.get_env("REBAR_DEPS_EBIN", ""), ":")
out_dir = System.get_env("REBAR_PROTOCOLS_OUTDIR", "")
File.mkdir_p!(out_dir)

# For protocol consolidation run the following script
# paths is a list of paths to dependency ebin directory
# output is the output directory for the compiled protocol beam files
Enum.each(paths, fn path ->
  Enum.each(Protocol.extract_protocols([path]), fn protocol ->
    impls = Protocol.extract_impls(protocol, paths)
    :code.purge(protocol)
    :code.delete(protocol)
    File.cd!(path)
    IO.puts("Consolidating #{length(impls)} implementations of protocol #{protocol}")
    {:ok, beam} = Protocol.consolidate(protocol, impls)
    File.write!(Path.join(out_dir, "#{protocol}.beam"), beam)
  end)
end)
