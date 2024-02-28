/** @type {import('next').NextConfig} */
const nextConfig = {
  images: {
    domains: [
      "res.cloudinary.com",
      "oaidalleapiprodscus.blob.core.windows.net",
      "img.clerk.com",
    ],
  },
  webpack: (config, { webpack }) => {
    config.plugins.push(
      new webpack.IgnorePlugin({
        resourceRegExp: /original-fs/,
        contextRegExp: /adm-zip/,
      })
    );
    config.plugins.push(
      new webpack.IgnorePlugin({
        resourceRegExp: /zipfile/,
        contextRegExp: /epub2/,
      })
    );
    return config;
  },
  async headers() {
    return [
      {
        source: "/",
        headers: [
          {
            key: "Permissions-Policy",
            value: "browsing-topics=()",
          },
        ],
      },
    ];
  },
};

module.exports = nextConfig;
