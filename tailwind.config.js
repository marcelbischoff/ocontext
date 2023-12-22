module.exports = {
  // ...other settings
  theme: {},
  plugins: [
    require("@tailwindcss/forms"),
    require("@catppuccin/tailwindcss")({
      // prefix to use, e.g. `text-pink` becomes `text-ctp-pink`.
      // default is `false`, which means no prefix
      prefix: "ctp",
      // which flavour of colours to use by default, in the `:root`
      defaultFlavour: "mocha",
    }),
  ],
  content: ["./**/*.html"],
};
