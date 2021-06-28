// Snowpack Configuration File
// See all supported options: https://www.snowpack.dev/reference/configuration

/** @type {import("snowpack").SnowpackUserConfig } */
module.exports = {
    mount: {
        "src": "/_dist_",
        "public": "/"
    },
    plugins: [
        "snowpack-plugin-elm",
        "@snowpack/plugin-dotenv"
    ],
    devOptions: {
        port: 3000
    },
};
