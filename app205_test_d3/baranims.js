// file copied from https://github.com/rstudio/r2d3/tree/main/inst/examples

var barHeight = Math.floor(height / data.length);

var bars = r2d3.svg.selectAll('rect')
    .data(r2d3.data);

bars.enter()
    .append('rect')
      .attr('width', function(d) { return d * width; })
      .attr('height', barHeight)
      .attr('y', function(d, i) { return i * barHeight; })
      .attr('fill', 'steelblue');

bars.exit().remove();

bars.transition()
  .duration(300)
  .attr("width", function(d) { return d * width; });
  