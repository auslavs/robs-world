import { defineConfig } from "vite";

export default defineConfig({
  base: "./",
  plugins: [],
  build: {
    outDir: "docs",
    emptyOutDir: true,
    sourcemap: true
  }
});