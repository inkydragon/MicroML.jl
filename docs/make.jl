using MicroML
using Documenter

DocMeta.setdocmeta!(MicroML, :DocTestSetup, :(using MicroML); recursive=true)

makedocs(;
    modules=[MicroML],
    repo=Remotes.GitHub("inkydragon", "MicroML.jl"),
    authors="Chengyu HAN <cyhan.dev@outlook.com> and contributors",
    sitename="MicroML.jl",
    format=Documenter.HTML(;
        repolink="https://inkydragon.github.io/MicroML.jl",
        canonical="https://inkydragon.github.io/MicroML.jl",
        edit_link="main",
        assets=String[],
    ),
    pages=Any[
        "index.md",
    ],
    checkdocs=:exports,
)

deploydocs(;
    repo="github.com/inkydragon/MicroML.jl",
    devbranch="main",
)
