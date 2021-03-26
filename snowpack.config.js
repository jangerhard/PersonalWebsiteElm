// Snowpack Configuration File
// See all supported options: https://www.snowpack.dev/reference/configuration

NODE_VERSION = '15.0.1'
NPM_VERSION = '7.6.1'

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
    packageOptions: {
        /* ... */
    },
    devOptions: {
        port: 3000
    },
    buildOptions: {
        /* ... */
    },
};
