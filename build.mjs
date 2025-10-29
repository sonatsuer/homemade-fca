import esbuild from "esbuild"
import pursPlugin from "esbuild-plugin-purescript"
import copyStaticFiles from "esbuild-copy-static-files"

const ctx = await esbuild
  .context({
    entryPoints: ["src/index.js"],
    bundle: true,
    outdir: "dist",
    plugins: [
      pursPlugin(),
      copyStaticFiles({ src: "./static", dest: "./dist" })
    ],
    //logLevel: "debug"
  })
  .catch((e) => {
    console.error(e)
    process.exit(1)
  });

await ctx.watch()
await ctx.serve({ servedir: "./dist", port: 3000 })
